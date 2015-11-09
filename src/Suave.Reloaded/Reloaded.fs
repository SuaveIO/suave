module Suave.Reloaded

open System
open System.Diagnostics
open System.IO
open Suave
open Suave.Web
open Suave.Types
open Microsoft.FSharp.Compiler.Interactive.Shell
open Hopac

let help = """
You should use this module by loading it into e.g. a console app, or a fsx file.

You should move your app to a module.

The fsx file and the Program.fs files should both spawn the server using this
module which in turn calls the app module.
"""

[<Serializable>]
type ScriptConfig =
  { fsxPath    : FileInfo
    modulePath : string
    expression : string
    homeFolder : DirectoryInfo option }

type OnRunning = ScriptConfig -> unit

let launchBrowser _ : OnRunning = Process.Start("http://localhost:8083") |> ignore

let defaultScriptConfig =
  let startTheBrowser 
  { fsxPath    = FileInfo "App.fsx"
    modulePath = "App"
    expression = "app"
    homeFolder = None
  }

module internal OutsideAppDomain =
  open System.Text
  open System.Text.RegularExpressions
  open System.Threading
  open System
  open Suave.Logging

  let private count = ref 0

  // thanks http://www.superstarcoders.com/blogs/posts/executing-code-in-a-separate-application-domain-using-c-sharp.aspx
  type Isolated<'t when 't :> MarshalByRefObject> =
    val serialNo : int
    val domain   : AppDomain
    val typ      : Type
    val value    : 't

    new () =
      let serialNo = Interlocked.Increment count
      let domain = AppDomain.CreateDomain("Isolated#" + string serialNo, null, AppDomain.CurrentDomain.SetupInformation)
      let typ = typeof<'t>
      { serialNo = Interlocked.Increment(count)
        domain   = domain
        typ      = typ
        value    = domain.CreateInstanceAndUnwrap(typ.Assembly.FullName, typ.FullName) :?> 't }

    member x.Value = x.value

    interface IDisposable with
      member x.Dispose () =
        AppDomain.Unload x.domain

  let watcher (globPattern : string) : Hopac.Ch<FileSystemEventArgs> =
    let ch = ch ()
    ch

module internal InsideAppDomain =

  module Scripting =
    open System.Text
    open System.Threading
    open System
    open Suave.Logging
    open Suave.Http.ServerErrors

    let warn logger = Log.log logger "Suave.Reloaded.Scripting" LogLevel.Warn
    let info logger = Log.log logger "Suave.Reloaded.Scripting" LogLevel.Info

    type ScriptState =
      { session : FsiEvaluationSession
        out     : StringBuilder
        err     : StringBuilder
        app     : WebPart
        logger  : Logger }

      static member Create() =
        let sbOut = new Text.StringBuilder()
        let sbErr = new Text.StringBuilder()

        let inStream = new StringReader("")
        let outStream = new StringWriter(sbOut)
        let errStream = new StringWriter(sbErr)
        let fsiConfig = FsiEvaluationSession.GetDefaultConfiguration()
        let argv = Array.append [|"/fake/fsi.exe"; "--quiet"; "--noninteractive"; "-d:DO_NOT_START_SERVER"|] [||]
        FsiEvaluationSession.Create(fsiConfig, argv, inStream, outStream, errStream)

    let buildError (config : ScriptConfig) (state : ScriptState) (e:exn) : string =
      String.Format("""Reloading '{0}' script failed.

Message:
  {1}

Error:
  {2}""",
        config.fsxPath,
        e.Message,
        state.err.ToString().Trim())

    let reloadScript (config : ScriptConfig) (state : ScriptState) : ScriptState =
      try
        state.session.EvalInteraction(sprintf "#load @\"%s\"" config.fsxPath.FullName)
        state.session.EvalInteraction(sprintf "open %s" config.modulePath)
        match state.session.EvalExpression config.expression with
        | Some app ->
          { state with app = app.ReflectionValue :?> WebPart }

        | None ->
          failwith "Couldn't get 'app' value"

      with e ->
        let state' = ScriptState.Create()
        { state' with app = INTERNAL_ERROR (buildError config state e) }

    let serverConfig homeFolder =
      { defaultConfig with
          homeFolder = Some homeFolder //Some __SOURCE_DIRECTORY__
          logger     = Loggers.saneDefaultsFor LogLevel.Debug
          bindings   = [ HttpBinding.mk' HTTP  "127.0.0.1" 8083] }

    let scriptServer (config : ScriptConfig) : Alt<unit> =
      let rec reconfigure () = job {
        let home =
          config.homeFolder |> Option.fold (fun s t ->
            Path.GetDirectoryName t.fsxPath |> DirectoryInfo)

        let runningState = ScriptState.Create() |> reloadScript (serverConfig homeFolder)
        let _, server = startWebServerAsync serverConfig state.app
        let runningServer = Hopac.Extensions.Async.toAlt server |> queue
        return! loop (runningServer, runningState)
      }
      and loop (runningServer, state) : Alt<unit> =
        let changes = watcher "*.*"
        Alt.choose [ changes >>% reconfigure
                     cancellation ]

      reconfigure ()

  module DLL =
    ()


type Initial() =
  inherit MarshalByRefObject
  member Start (config : ScriptConfig) : Running =
    InsideAppDomain.Scripting.scriptServer config
    |> Running

and Running(server : Alt<unit>) =
  inherit MarshalByRefObject
  interface IDisposable with
    member x.Dispose() =
      Alt.nack server |> run

let startWebServer (config : ScriptConfig) =
  use isolated = new Isolated<Initial>()
  use handle = isolated.Value.Start config
  Console.ReadKey(true) |> ignore