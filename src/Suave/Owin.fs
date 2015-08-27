module Suave.Owin

// Following the specification:
// https://github.com/owin/owin/blob/master/spec/owin-1.1.0.md

open System
open System.Net
open System.Threading
open System.Collections.Generic
open System.Threading.Tasks
open System.Security.Claims
open System.Security.Principal

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

  open Suave.Utils.Aether
  open Suave.Utils.Aether.Operators
  open Suave.Utils
  open Suave.Sockets.Control
  open Suave.Web.ParsingAndControl

  type OwinRequest =
    abstract OnSendingHeadersAction : Action<Action<obj>, obj>

  let untyped<'t> : Iso<'t, obj> =
    box, unbox

  let inline stringlyTyped (convertBack : string -> 'a) : Property<'a, string> =
    string, fun v x -> convertBack v

  type private Clock = uint64

  type Muuutation(initialHeaders : Map<string, string[]>) =
    let changed : Map<string, Clock * string []> ref = ref Map.empty
    let removed : Map<string, Clock> ref = ref Map.empty
    let mutable clock = 1UL

    member x.Delta =
      let keys =
        let changed = !changed |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        let removed = !removed |> Map.toSeq |> Seq.map fst |> Set.ofSeq
        Set.union changed removed

      let decide headers key =
        match !removed |> Map.tryFind key with
        | Some rmCl ->
          match !changed |> Map.tryFind key with
          | Some (chCl, value) ->
            if chCl > rmCl then
              headers |> Map.put key value
            else
              headers |> Map.remove key

          | None ->
            headers |> Map.remove key
            
        | None ->
          headers |> Map.put key (!changed |> Map.find key |> snd)
          
      keys |> Seq.fold decide initialHeaders

    interface IDictionary<string, string[]> with
      member x.Item
        with get key =
          match !removed |> Map.tryFind key with
          | Some rmCl ->
            match !changed |> Map.tryFind key with
            | Some (chCl, value) ->
              if chCl > rmCl then value else raise (KeyNotFoundException())

            | None ->
              raise (KeyNotFoundException())

          | None ->
            !changed
            |> Map.tryFind key
            |> Option.fold (fun s t -> snd t) (initialHeaders |> Map.find key)

        and set key value =
          changed := !changed |> Map.put key (clock, value)
          clock <- clock + 1UL

      member x.Remove key =
        removed := !removed |> Map.put key clock
        clock <- clock + 1UL

  type internal DWr(initialState) =
    let state : HttpContext ref = ref initialState

    let req l =
      HttpContext.request_ >--> l <--> untyped

    let run l =
      HttpContext.runtime_ >--> l <--> untyped

    let res l =
      HttpContext.response_ >--> l <--> untyped

    let uriAbsolutePath : Property<_, _> =
      (fun (uri : Uri) -> uri.AbsolutePath),
      (fun v uri -> UriBuilder(uri, Path = v).Uri)

    let hv2p : Property<string, string> =
      (function | "2.0" -> "HTTP/2.0"
                | "1.0" -> "HTTP/1.0"
                | _ | "1.1" -> "HTTP/1.1"),
      (fun v -> function | "HTTP/2.0" -> "2.0"
                         | "HTTP/1.0" -> "1.0"
                         | "HTTP/1.1" -> "1.1"
                         | x -> x)

    // as we say in Swedish; slafsigt!
    let mutableHeaders : Property<(string * string) list, Muuutation> =
      (fun x -> Muuutation(x |> List.map (fun (key, value) -> key, [| value |]) |> Map.ofList)),
      (fun v x -> v.Delta |> Map.toList |> List.map (fun (k, vs) -> k, String.concat ", " vs))

    let bytesToStream : Property<byte[], IO.Stream> =
      failwith "TODO"

    let claimsPrincipal : Property<Map<string, obj>, ClaimsPrincipal> =
      (fun x ->
        x
        |> Map.tryFind "principal" // TODO: add support for ClaimsPrincipal in Core
        |> function
        | Some x -> unbox x
        | None -> raise (KeyNotFoundException()) // spec doesn't say
      ),
      (fun v x -> x |> Map.put "principal" (box v))

    let i2sc : Property<HttpCode, int> =
      (fun x -> x.code),
      (fun v x -> HttpCode.TryParse v |> Option.get) // NOTE: assumes user only sets valid codes

    let owinMap =
      [ (* 3.2.1 Request Data *)
        OwinConstants.requestScheme, req HttpRequest.httpVersion_
        OwinConstants.requestMethod, req HttpRequest.method_
        OwinConstants.requestPathBase, run HttpRuntime.homeDirectory_
        OwinConstants.requestPath, HttpContext.request_ >--> HttpRequest.url_ >--> uriAbsolutePath <--> untyped
        OwinConstants.requestQueryString, req HttpRequest.rawQuery_
        OwinConstants.requestProtocol, HttpContext.request_ >--> HttpRequest.httpVersion_ >--> hv2p <--> untyped
        OwinConstants.requestHeaders, HttpContext.request_ >--> HttpRequest.headers_ >--> mutableHeaders <--> untyped
        OwinConstants.requestBody, HttpContext.request_ >--> HttpRequest.rawForm_ >--> bytesToStream <--> untyped
        OwinConstants.requestId, HttpContext.request_ >--> HttpRequest.trace_ >--> Logging.TraceHeader.traceId_ >--> stringlyTyped uint64 <--> untyped
        OwinConstants.requestUser, HttpContext.userState_ >--> claimsPrincipal <--> untyped

        (* 3.2.2 Response Data *)
        OwinConstants.responseStatusCode, HttpContext.response_ >--> HttpResult.status_ >--> i2sc <--> untyped
        OwinConstants.responseReasonPhrase, HttpContext.response_ <--> untyped // TODO
        OwinConstants.responseProtocol, HttpContext.response_ <--> untyped // TODO
        OwinConstants.responseHeaders, HttpContext.response_ <--> untyped // TODO
        OwinConstants.responseBody, HttpContext.response_ <--> untyped // TODO

        (* 3.2.3 Other Data *)
        OwinConstants.callCancelled, HttpContext.response_ <--> untyped // TODO
        OwinConstants.owinVersion, HttpContext.response_ <--> untyped // TODO
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
        state := Lens.set (owinRW |> Map.find k) v !state

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
