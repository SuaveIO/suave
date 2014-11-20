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
  let [<Literal>] request_scheme = "owin.RequestScheme"
  [<CompiledName ("RequestMethod")>]
  let [<Literal>] request_method = "owin.RequestMethod"
  [<CompiledName ("RequestPathBase")>]
  let [<Literal>] request_path_base = "owin.RequestPathBase"
  [<CompiledName ("RequestPath")>]
  let [<Literal>] request_path = "owin.RequestPath"
  [<CompiledName ("RequestQueryString")>]
  let [<Literal>] request_query_string = "owin.RequestQueryString"
  [<CompiledName ("RequestProtocol")>]
  let [<Literal>] request_protocol = "owin.RequestProtocol"
  [<CompiledName ("RequestHeaders")>]
  let [<Literal>] request_headers = "owin.RequestHeaders"
  [<CompiledName ("RequestBody")>]
  let [<Literal>] request_body = "owin.RequestBody"
  [<CompiledName ("RequestId")>]
  let [<Literal>] request_id = "owin.RequestId"
  [<CompiledName ("RequestUser")>]
  let [<Literal>] request_user = "owin.RequestUser"

  (* 3.2.2 Response Data *)
  [<CompiledName ("ResponseStatusCode")>]
  let [<Literal>] response_status_code = "owin.ResponseStatusCode"
  [<CompiledName ("ResponseReasonPhrase")>]
  let [<Literal>] response_reason_phrase = "owin.ResponseReasonPhrase"
  [<CompiledName ("ResponseProtocol")>]
  let [<Literal>] response_protocol = "owin.ResponseProtocol"
  [<CompiledName ("ResponseHeaders")>]
  let [<Literal>] response_headers = "owin.ResponseHeaders"
  [<CompiledName ("ResponseBody")>]
  let [<Literal>] response_body = "owin.ResponseBody"

  (* 3.2.3 Other Data *)
  [<CompiledName ("CallCancelled")>]
  let [<Literal>] call_cancelled = "owin.CallCancelled"
  [<CompiledName ("OwinVersion")>]
  let [<Literal>] owin_version = "owin.Version"

  (* http://owin.org/spec/CommonKeys.html *)
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CommonKeys =
    [<CompiledName ("ClientCertificate")>]
    let [<Literal>] client_certificate = "ssl.ClientCertificate"
    [<CompiledName ("TraceOutput")>]
    let [<Literal>] trace_output = "host.TraceOutput"
    [<CompiledName ("Addresses")>]
    let [<Literal>] addresses = "host.Addresses"
    [<CompiledName ("RemoteIpAddress")>]
    let [<Literal>] remote_ip_address = "server.RemoteIpAddress"
    [<CompiledName ("RemotePort")>]
    let [<Literal>] remote_port = "server.RemotePort"
    [<CompiledName ("LocalIpAddress")>]
    let [<Literal>] local_ip_address = "server.LocalIpAddress"
    [<CompiledName ("LocalPort")>]
    let [<Literal>] local_port = "server.LocalPort"
    [<CompiledName ("IsLocal")>]
    let [<Literal>] is_local = "server.IsLocal"
    [<CompiledName ("Capabilities")>]
    let [<Literal>] capabilities = "server.Capabilities"
    [<CompiledName ("ServerName")>]
    let [<Literal>] server_name = "server.Name"
    [<CompiledName ("OnSendingHeaders")>]
    let [<Literal>] on_sending_headers = "server.OnSendingHeaders"

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
    let [<Literal>] send_async = "sendfile.SendAsync"

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
    let [<Literal>] call_cancelled = "opaque.CallCancelled"

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
    let [<Literal>] sub_protocol = "websocket.SubProtocol"

    // 5. Consumption
    [<CompiledName ("SendAsync")>]
    let [<Literal>] send_async = "websocket.SendAsync"
    [<CompiledName ("ReceiveAsync")>]
    let [<Literal>] receive_async = "websocket.ReceiveAsync"
    [<CompiledName ("CloseAsync")>]
    let [<Literal>] close_async = "websocket.CloseAsync"
    [<CompiledName ("CallCancelled")>]
    let [<Literal>] call_cancelled = "websocket.CallCancelled"
    [<CompiledName ("ClientCloseStatus")>]
    let [<Literal>] client_close_status = "websocket.ClientCloseStatus"
    [<CompiledName ("ClientCloseDescription")>]
    let [<Literal>] client_close_description = "websocket.ClientCloseDescription"

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =

  let private wrap (ctx : HttpContext) : OwinEnvironment =
      dict [] // TODO

  [<CompiledName ("ToSuave")>]
  let to_suave (owin : OwinApp) : WebPart =
    fun (ctx : HttpContext) ->

      let impl conn : SocketOp<unit> = async {
        do! owin (wrap ctx)
        // todo: on_sending_headers
        // todo: http serving
        return Choice1Of2 ()
        }

      { ctx with
          response =
            { ctx.response with
                content = SocketTask impl
            }
      }
      |> succeed

  let to_suave' (owin : OwinAppFunc) =
    to_suave (fun e -> Async.AwaitTask ((owin.Invoke e).ContinueWith<_>(fun _ -> ())))

module OwinServerFactory =

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
  let initialize (props : OwinEnvironment) =
    if props = null then nullArg "props"
    props.[OwinConstants.owin_version] <- "1.1"
    let cap = ``read_env!`` props OwinConstants.CommonKeys.capabilities
    cap.[OwinConstants.CommonKeys.server_name] <- Globals.Internals.server_name

  [<CompiledName ("Create")>]
  let create (app : OwinAppFunc, props : OwinEnvironment) =
    if app = null then nullArg "app"
    if props = null then nullArg "props"

    let cap = ``read_env!`` props OwinConstants.CommonKeys.capabilities
    let bindings =
      (``read_list!`` props OwinConstants.CommonKeys.addresses
       : IList<OwinEnvironment>)
      |> Seq.map (fun dic ->
        let port   = get dic "port" : string
        let ip     = read dic "ip" (fun () -> "127.0.0.1") : string
        let scheme = get_default dic "certificate" (fun _ -> HTTP) HTTP
        { scheme = scheme; socketBinding = { ip = IPAddress.Parse ip; port = uint16 port } })
      |> List.ofSeq

    let cts = new CancellationTokenSource()

    let conf =
      { Web.defaultConfig with
          bindings          = bindings
          cancellationToken = cts.Token }

    let started, listening =
      Web.startWebServerAsync conf (OwinAppFunc.to_suave' app)

    let _ = started |> Async.RunSynchronously

    { new IDisposable with
      member x.Dispose () =
        // note: this won't let the web requests finish gently
        cts.Cancel()
        cts.Dispose()
      }
