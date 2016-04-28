namespace Suave.Examples.WebSharper
#nowarn "44"

module SampleSite =
  open WebSharper
  open WebSharper.Web
  open WebSharper.Sitelets
  open WebSharper.UI.Next.Html

  type EndPoint =
    | Index

  let IndexContent context : Async<Content<EndPoint>> =
    let time = System.DateTime.Now.ToString()
    Content.Page(
      Title = "Index",
      Body = [h1 [text ("Current time: " + time)]]
    )

  [<Website>]
  let MySampleWebsite : Sitelet<EndPoint> =
    Sitelet.Content "/index" EndPoint.Index IndexContent

// NOTE: WebSharper has a custom Suave module that's more WebSharper-ideomatic;
// at https://github.com/intellifactory/websharper.suave/tree/master/WebSharper.Suave.Tests
// but the below is a display that we can run WebSharper only based on the OWIN
// spec.

module SelfHostedServer =

  open global.Owin
  open System.Net
  open System
  open System.IO
  open System.Diagnostics
  open Suave
  open Suave.Web
  open Suave.Logging
  open Suave.Owin
  open WebSharper.Owin

  open System.Threading.Tasks

  let logger = Loggers.saneDefaultsFor LogLevel.Verbose

  let exitOwin (env:OwinEnvironment) =
    async {
      Log.verbose logger "owin" TraceHeader.empty "exit WebSharper middleware."
    }
    |> Async.StartAsTask
    :> Task

  [<EntryPoint>]
  let Main argv =

    let app =
      let root = "../.."
      let binDir = typeof<SiteletMiddleware<_>>.Assembly.Location
                |> Path.GetDirectoryName
      let options = Options.Create(root,binDir)
      OwinApp.ofAppFunc "" <| AppFunc(SiteletMiddleware(AppFunc(exitOwin), options, SampleSite.MySampleWebsite).Invoke)

    let config =
      { defaultConfig with
          logger = logger }

    startWebServer config <| choose [ app; RequestErrors.NOT_FOUND "file not found." ]
    0
