namespace Suave

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinConstants =
  (* 3.2.1 Request Data *)
  let [<Literal>] request_scheme = "owin.RequestScheme"
  let [<Literal>] request_method = "owin.RequestMethod"
  let [<Literal>] request_path_base = "owin.RequestPathBase"
  let [<Literal>] request_path = "owin.RequestPath"
  let [<Literal>] request_query_string = "owin.RequestQueryString"
  let [<Literal>] request_protocol = "owin.RequestProtocol"
  let [<Literal>] request_headers = "owin.RequestHeaders"
  let [<Literal>] request_body = "owin.RequestBody"
  let [<Literal>] request_id = "owin.RequestId"
  let [<Literal>] request_user = "owin.RequestUser"

  (* 3.2.2 Response Data *)
  let [<Literal>] response_status_code = "owin.ResponseStatusCode"
  let [<Literal>] response_reason_phrase = "owin.ResponseReasonPhrase"
  let [<Literal>] response_protocol = "owin.ResponseProtocol"
  let [<Literal>] response_headers = "owin.ResponseHeaders"
  let [<Literal>] response_body = "owin.ResponseBody"

  (* 3.2.3 Other Data *)
  let [<Literal>] call_cancelled = "owin.CallCancelled"
  let [<Literal>] owin_version = "owin.Version"

  (* http://owin.org/spec/CommonKeys.html *)
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CommonKeys =
    let [<Literal>] client_certificate = "ssl.ClientCertificate"
    let [<Literal>] remote_ip_address = "server.RemoteIpAddress"
    let [<Literal>] remote_port = "server.RemotePort"
    let [<Literal>] local_ip_address = "server.LocalIpAddress"
    let [<Literal>] local_port = "server.LocalPort"
    let [<Literal>] is_local = "server.IsLocal"
    let [<Literal>] trace_output = "host.TraceOutput"
    let [<Literal>] addresses = "host.Addresses"
    let [<Literal>] capabilities = "server.Capabilities"
    let [<Literal>] on_sending_headers = "server.OnSendingHeaders"

  (* http://owin.org/extensions/owin-SendFile-Extension-v0.3.0.htm *)
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module SendFiles =
    // 3.1. Startup
    let [<Literal>] version = "sendfile.Version"
    let [<Literal>] support = "sendfile.Support"
    let [<Literal>] concurrency = "sendfile.Concurrency"

    // 3.2. Per Request
    let [<Literal>] send_async = "sendfile.SendAsync"

  (* http://owin.org/extensions/owin-OpaqueStream-Extension-v0.3.0.htm *)
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Opaque =
    // 3.1. Startup
    let [<Literal>] version = "opaque.Version"

    // 3.2. Per Request
    let [<Literal>] upgrade = "opaque.Upgrade"

    // 5. Consumption
    let [<Literal>] stream = "opaque.Stream"
    let [<Literal>] call_cancelled = "opaque.CallCancelled"

  (* http://owin.org/extensions/owin-WebSocket-Extension-v0.4.0.htm *)
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module WebSocket =
    // 3.1. Startup
    let [<Literal>] version = "websocket.Version"

    // 3.2. Per Request
    let [<Literal>] accept = "websocket.Accept"

    // 4. Accept
    let [<Literal>] sub_protocol = "websocket.SubProtocol"

    // 5. Consumption
    let [<Literal>] send_async = "websocket.SendAsync"
    let [<Literal>] receive_async = "websocket.ReceiveAsync"
    let [<Literal>] close_async = "websocket.CloseAsync"
    let [<Literal>] call_cancelled = "websocket.CallCancelled"
    let [<Literal>] client_close_status = "websocket.ClientCloseStatus"
    let [<Literal>] client_close_description = "websocket.ClientCloseDescription"
