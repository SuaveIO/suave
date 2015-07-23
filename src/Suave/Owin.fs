module Suave.Owin

// Following the specification:
// https://github.com/owin/owin/blob/master/spec/owin-1.1.0.md

open System
open System.Net
open System.Threading
open System.Collections.Generic
open System.Threading.Tasks

open Suave
open Suave.Http
open Suave.Types
open Suave.Sockets

type OwinEnvironment =
  IDictionary<string, obj>

type OwinApp =
  OwinEnvironment -> Async<unit>

type OwinAppFunc =
  Func<OwinEnvironment, Task>

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinConstants =
  (* 3.2.1 Request Data *)
  [<CompiledName ("RequestScheme")>]
  let [<Literal>] requestScheme = "owin.RequestScheme"
  [<CompiledName ("RequestMethod")>]
  let [<Literal>] requestMethod = "owin.RequestMethod"
  [<CompiledName ("RequestPathBase")>]
  let [<Literal>] requestPathBase = "owin.RequestPathBase"
  [<CompiledName ("RequestPath")>]
  let [<Literal>] requestPath = "owin.RequestPath"
  [<CompiledName ("RequestQueryString")>]
  let [<Literal>] requestQueryString = "owin.RequestQueryString"
  [<CompiledName ("RequestProtocol")>]
  let [<Literal>] requestProtocol = "owin.RequestProtocol"
  [<CompiledName ("RequestHeaders")>]
  let [<Literal>] requestHeaders = "owin.RequestHeaders"
  [<CompiledName ("RequestBody")>]
  let [<Literal>] requestBody = "owin.RequestBody"
  [<CompiledName ("RequestId")>]
  let [<Literal>] requestId = "owin.RequestId"
  [<CompiledName ("RequestUser")>]
  let [<Literal>] requestUser = "owin.RequestUser"

  (* 3.2.2 Response Data *)
  [<CompiledName ("ResponseStatusCode")>]
  let [<Literal>] responseStatusCode = "owin.ResponseStatusCode"
  [<CompiledName ("ResponseReasonPhrase")>]
  let [<Literal>] responseReasonPhrase = "owin.ResponseReasonPhrase"
  [<CompiledName ("ResponseProtocol")>]
  let [<Literal>] responseProtocol = "owin.ResponseProtocol"
  [<CompiledName ("ResponseHeaders")>]
  let [<Literal>] responseHeaders = "owin.ResponseHeaders"
  [<CompiledName ("ResponseBody")>]
  let [<Literal>] responseBody = "owin.ResponseBody"

  (* 3.2.3 Other Data *)
  [<CompiledName ("CallCancelled")>]
  let [<Literal>] callCancelled = "owin.CallCancelled"
  [<CompiledName ("OwinVersion")>]
  let [<Literal>] owinVersion = "owin.Version"

  (* http://owin.org/spec/CommonKeys.html *)
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CommonKeys =
    [<CompiledName ("ClientCertificate")>]
    let [<Literal>] clientCertificate = "ssl.ClientCertificate"
    [<CompiledName ("TraceOutput")>]
    let [<Literal>] traceOutput = "host.TraceOutput"
    [<CompiledName ("Addresses")>]
    let [<Literal>] addresses = "host.Addresses"
    [<CompiledName ("RemoteIpAddress")>]
    let [<Literal>] remoteIpAddress = "server.RemoteIpAddress"
    [<CompiledName ("RemotePort")>]
    let [<Literal>] remotePort = "server.RemotePort"
    [<CompiledName ("LocalIpAddress")>]
    let [<Literal>] localIpAddress = "server.LocalIpAddress"
    [<CompiledName ("LocalPort")>]
    let [<Literal>] localPort = "server.LocalPort"
    [<CompiledName ("IsLocal")>]
    let [<Literal>] isLocal = "server.IsLocal"
    [<CompiledName ("Capabilities")>]
    let [<Literal>] capabilities = "server.Capabilities"
    [<CompiledName ("ServerName")>]
    let [<Literal>] serverName = "server.Name"
    [<CompiledName ("OnSendingHeaders")>]
    let [<Literal>] onSendingHeaders = "server.OnSendingHeaders"

  (* http://owin.org/extensions/owin-SendFile-Extension-v0.3.0.htm *)
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module SendFiles =
    // 3.1. Startup
    [<CompiledName ("Version")>]
    let [<Literal>] version = "sendfile.Version"
    [<CompiledName ("Support")>]
    let [<Literal>] support = "sendfile.Support"
    [<CompiledName ("Concurrency")>]
    let [<Literal>] concurrency = "sendfile.Concurrency"

    // 3.2. Per Request
    [<CompiledName ("SendAsync")>]
    let [<Literal>] sendAsync = "sendfile.SendAsync"

  (* http://owin.org/extensions/owin-OpaqueStream-Extension-v0.3.0.htm *)
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Opaque =
    // 3.1. Startup
    [<CompiledName ("Version")>]
    let [<Literal>] version = "opaque.Version"

    // 3.2. Per Request
    [<CompiledName ("Upgrade")>]
    let [<Literal>] upgrade = "opaque.Upgrade"

    // 5. Consumption
    [<CompiledName ("Stream")>]
    let [<Literal>] stream = "opaque.Stream"
    [<CompiledName ("CallCanceled")>]
    let [<Literal>] callCancelled = "opaque.CallCancelled"

  (* http://owin.org/extensions/owin-WebSocket-Extension-v0.4.0.htm *)
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module WebSocket =
    // 3.1. Startup
    [<CompiledName ("Version")>]
    let [<Literal>] version = "websocket.Version"

    // 3.2. Per Request
    [<CompiledName ("Accept")>]
    let [<Literal>] accept = "websocket.Accept"

    // 4. Accept
    [<CompiledName ("SubProtocol")>]
    let [<Literal>] subProtocol = "websocket.SubProtocol"

    // 5. Consumption
    [<CompiledName ("SendAsync")>]
    let [<Literal>] sendAsync = "websocket.SendAsync"
    [<CompiledName ("ReceiveAsync")>]
    let [<Literal>] receiveAsync = "websocket.ReceiveAsync"
    [<CompiledName ("CloseAsync")>]
    let [<Literal>] closeAsync = "websocket.CloseAsync"
    [<CompiledName ("CallCancelled")>]
    let [<Literal>] callCancelled = "websocket.CallCancelled"
    [<CompiledName ("ClientCloseStatus")>]
    let [<Literal>] clientCloseStatus = "websocket.ClientCloseStatus"
    [<CompiledName ("ClientCloseDescription")>]
    let [<Literal>] clientCloseDescription = "websocket.ClientCloseDescription"

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =

  open Suave.Utils
  open Suave.Sockets.Control
  open Suave.Web.ParsingAndControl

  type OwinRequest =
    abstract OnSendingHeadersAction : Action<Action<obj>, obj>

  // steal from https://github.com/xyncro/aether/blob/master/src/Aether/Aether.fs#L30

  /// Total isomorphism of a <> b
  type Iso<'a,'b> = ('a -> 'b) * ('b -> 'a)

  /// Partial isomorphism of a <> b
  type PIso<'a,'b> = ('a -> 'b option) * ('b -> 'a)

  /// Compose a total lens and a total lens, giving a total lens
  let totalLensTotalLens ((g1, s1): Property<'a,'b>) ((g2, s2): Property<'b,'c>) : Property<'a,'c> =
      (fun a -> g2 (g1 a)),
      (fun c a -> s1 (s2 c (g1 a)) a)

  /// Compose a total lens with a total isomorphism, giving a total lens
  let totalLensTotalIsomorphism ((g, s): Property<'a,'b>) ((f, t): Iso<'b,'c>) : Property<'a,'c> =
      (fun a -> f (g a)),
      (fun c a -> s (t c) a)

  /// Compose a total lens with a partial isomorphism, giving a partial lens
  let totalLensPartialIsomorphism ((g, s): Property<'a,'b>) ((f, t): PIso<'b,'c>) : PLens<'a,'c> =
      (fun a -> f (g a)),
      (fun c a -> s (t c) a)
  /// Compose a total lens and a total lens, giving a total lens
  let (>-->) l1 l2 =
      totalLensTotalLens l1 l2

  /// Compose a total lens with a total isomorphism, giving a total lens
  let (<-->) l i =
      totalLensTotalIsomorphism l i

  /// Compose a total lens with a partial isomorphism, giving a partial lens
  let (<-?>) l i =
      totalLensPartialIsomorphism l i

  let isoBox<'t> : Iso<'t, obj> =
    box, unbox

  type internal DWr(initialState) =
    let state : HttpContext ref = ref initialState

    let req l = HttpContext.request_ >--> l <--> isoBox
    let run l = HttpContext.runtime_ >--> l <--> isoBox
    let uriAbsolutePath : Property<_, _> =
      (fun (uri : Uri) -> uri.AbsolutePath),
      (fun v uri -> UriBuilder(uri, Path = v).Uri)

    // TODO: use lenses instead; it would be nicer
    let owinMap =
      [ (* 3.2.1 Request Data *)
        OwinConstants.requestScheme, req HttpRequest.httpVersion_
        OwinConstants.requestMethod, req HttpRequest.method_
        OwinConstants.requestPathBase, run HttpRuntime.homeDirectory_
        OwinConstants.requestPath, HttpContext.request_ >--> HttpRequest.url_ >--> uriAbsolutePath <--> isoBox

        // @panesofglass: continue from here, wrapping it all!
        OwinConstants.requestQueryString, fun ctx -> ctx.request.rawQuery |> box
        OwinConstants.requestProtocol, fun ctx -> "HTTP" |> box
        OwinConstants.requestHeaders, fun ctx -> ctx.request.headers |> Map.ofList :> IDictionary<string,string> |> box
        OwinConstants.requestBody, fun ctx -> ctx.request.rawForm |> box
        OwinConstants.requestId, fun ctx -> ctx.request.trace.reqId |> box
        OwinConstants.requestUser, fun ctx -> ctx.userState |> Map.tryFind "user" |> function | Some x -> box x | None -> box null

        (* 3.2.2 Response Data *)
        OwinConstants.responseStatusCode, // etc, wrap in the lenses
        OwinConstants.responseReasonPhrase
        OwinConstants.responseProtocol
        OwinConstants.responseHeaders
        OwinConstants.responseBody

        (* 3.2.3 Other Data *)
        OwinConstants.callCancelled
        OwinConstants.owinVersion
      ]

    let owinKeys = owinMap |> List.map fst |> Set.ofSeq
    let owinRW   = owinMap |> Map.ofList

    member x.Interface =
      x :> IDictionary<string, obj>

    member x.State =
      state

    interface OwinRequest with
      member x.OnSendingHeadersAction =
        Action<_, _>(fun a -> fun x -> ())

    interface IDictionary<string, obj> with

      member x.Add (k, v) =
        // when you 'add' a key, you have to change the state for that key, in
        // Aether notation:
        // set the state to the 
        state := Lens.setLens (owinRW |> Map.find k) !state v

      member x.Remove k =
        // does it get removed before added?
        // in that case you need to keep track of the last value in the container
        // of the value you're removing, so that it may be updated with the lens
        // on Add
        ()

      member x.ContainsKey k =
        // we have ALL THE KEYS!!!
        owinKeys.Contains k

  let private wrap (ctx : HttpContext) =
    DWr ctx

  [<CompiledName "ToSuave">]
  let ofOwin (owin : OwinApp) : WebPart =
    fun (ctx : HttpContext) ->
      let impl conn : SocketOp<unit> = socket {
        let wrapper = wrap ctx
        do! SocketOp.ofAsync (owin wrapper.Interface)
        let ctx = !wrapper.State
        // if wrapper has OnHeadersSend, call that now => a new HttpContext (possibly), assign to ctx
        do! Web.ParsingAndControl.writePreamble ctx
        do! Web.ParsingAndControl.writeContent ctx ctx.response.content
      }

      { ctx with
          response =
            { ctx.response with
                content = SocketTask impl
                writePreamble = false
            }
      }
      |> succeed

  let ofOwinFunc (owin : OwinAppFunc) =
    ofOwin (fun e -> Async.AwaitTask ((owin.Invoke e).ContinueWith<_>(fun _ -> ())))

module OwinServerFactory =

  type Dic<'Key, 'Value> = IDictionary<'Key, 'Value>

  let private read (d : OwinEnvironment) k f =
    match d.TryGetValue k with
    | false, _ -> f ()
    | true, value -> value :?> 'a

  let private ``read_env!`` (e : OwinEnvironment) key =
    let res = read e key (fun () -> new Dictionary<string, obj>() :> OwinEnvironment)
    e.[key] <- res
    res

  let private ``read_dic!`` (e : OwinEnvironment) key =
    let res = read e key (fun () -> new Dictionary<string, 'a>())
    e.[key] <- res
    res

  let private ``read_list!`` (e : OwinEnvironment) key =
    let res = read e key (fun () -> new List<'a>() :> IList<'a>)
    e.[key] <- res
    res

  let private get (e : OwinEnvironment) key =
    match e.TryGetValue key with
    | false, _ -> failwithf "missing value for key '%s'" key
    | true, value -> value :?> 'a

  let private get_default (e : OwinEnvironment) key map defaults =
    match e.TryGetValue key with
    | false, _ -> defaults
    | true, value -> map (value :?> 'a)

  [<CompiledName "Initialize">]
  let initialize (props : Dic<string, obj>) =
    if props = null then nullArg "props"
    props.[OwinConstants.owinVersion] <- "1.0.1"
    let cap = ``read_env!`` props OwinConstants.CommonKeys.capabilities
    cap.[OwinConstants.CommonKeys.serverName] <- Globals.Internals.server_name

  [<CompiledName ("Create")>]
  let create (app : OwinAppFunc, props : Dic<string, obj>) =
    if app = null then nullArg "app"
    if props = null then nullArg "props"

    let bindings =
      (``read_list!`` props OwinConstants.CommonKeys.addresses
       : IList<OwinEnvironment>)
      |> Seq.map (fun dic ->
        let port   = get dic "port" : string
        let ip     = read dic "ip" (fun () -> "127.0.0.1") : string
        let scheme = get_default dic "certificate" (fun _ -> HTTP) HTTP
        { scheme = scheme; socketBinding = { ip = IPAddress.Parse ip; port = uint16 port } })
      |> List.ofSeq

    let serverCts = new CancellationTokenSource()

    let conf =
      { Web.defaultConfig with
          bindings          = bindings
          cancellationToken = serverCts.Token }

    let started, listening =
      Web.startWebServerAsync conf (OwinAppFunc.ofOwinFunc app)

    let _ = started |> Async.RunSynchronously

    { new IDisposable with
      member x.Dispose () =
        // note: this won't let the web requests finish gently
        serverCts.Cancel()
        serverCts.Dispose()
      }
