module Suave.Owin

// Following the specification:
// https://github.com/owin/owin/blob/master/spec/owin-1.1.0.md

open System
open System.Net
open System.Threading
open System.Collections
open System.Collections.Generic
open System.Threading.Tasks
open System.Security.Claims
open System.Security.Principal
open System.Runtime.InteropServices
open Suave
open Suave.Logging
open Suave.Http
open Suave.Types
open Suave.Sockets
open Suave.Utils

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module OwinConstants =
  // 3.2.1 Request Data
  [<CompiledName ("RequestScheme")>]
  let [<Literal>] requestScheme = "owin.RequestScheme"

  [<CompiledName ("RequestMethod")>]
  let [<Literal>] requestMethod = "owin.RequestMethod"

  /// Servers may have the ability to map application delegates to some base path. For example, a server might have an application delegate configured to respond to requests beginning with "/my-app", in which case it should set the value of "owin.RequestPathBase" in the environment dictionary to "/my-app". If this server receives a request for "/my-app/foo", the “owin.RequestPath” value of the environment dictionary provided to the application configured to respond at "/my-app" should be "/foo".
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

  // 3.2.2 Response Data
  [<CompiledName ("ResponseStatusCode")>]
  let [<Literal>] responseStatusCode = "owin.ResponseStatusCode"

  [<CompiledName ("ResponseReasonPhrase")>]
  let [<Literal>] responseReasonPhrase = "owin.ResponseReasonPhrase"

  [<CompiledName ("ResponseProtocol")>]
  let [<Literal>] responseProtocol = "owin.ResponseProtocol"

  [<CompiledName ("ResponseHeaders")>]
  let [<Literal>] responseHeaders = "owin.ResponseHeaders"

  /// The server provides a response body Stream with the “owin.ResponseBody” key
  /// in the initial environment dictionary.  The headers, status code, reason
  /// phrase, etc., can be modified up until the first write to the response body
  /// stream.  Upon first write, the server validates and sends the headers.
  /// Applications MAY choose to buffer response data to delay the header
  /// finalization.
  ///
  /// Currently, Suave buffers the response data.
  [<CompiledName ("ResponseBody")>]
  let [<Literal>] responseBody = "owin.ResponseBody"

  // 3.2.3 Other Data
  [<CompiledName ("CallCancelled")>]
  let [<Literal>] callCancelled = "owin.CallCancelled"

  [<CompiledName ("OwinVersion")>]
  let [<Literal>] owinVersion = "owin.Version"

  /// http://owin.org/spec/CommonKeys.html
  /// http://owin.org/spec/spec/CommonKeys.html
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module CommonKeys =
    /// The client certificate provided during HTTPS SSL negotiation.
    /// Type: X509Certificate
    [<CompiledName ("ClientCertificate")>]
    let [<Literal>] clientCertificate = "ssl.ClientCertificate"

    /// A tracing output that may be provided by the host.
    /// Type: TextWriter
    [<CompiledName ("TraceOutput")>]
    let [<Literal>] traceOutput = "host.TraceOutput"

    /// A list of per-address server configuration. The following keys are defined with string values: scheme, host, port, path.
    /// Type: IList<IDictionary<string, object>>
    [<CompiledName ("Addresses")>]
    let [<Literal>] addresses = "host.Addresses"

    /// The IP Address of the remote client. E.g. 192.168.1.1 or ::1
    /// Type: String
    [<CompiledName ("RemoteIpAddress")>]
    let [<Literal>] remoteIpAddress = "server.RemoteIpAddress"

    /// The port of the remote client. E.g. 1234
    /// Type: String
    [<CompiledName ("RemotePort")>]
    let [<Literal>] remotePort = "server.RemotePort"

    /// The local IP Address the request was received on. E.g. 127.0.0.1 or ::1
    /// Type: string
    [<CompiledName ("LocalIpAddress")>]
    let [<Literal>] localIpAddress = "server.LocalIpAddress"

    /// The port the request was received on. E.g. 80
    /// Type: String
    [<CompiledName ("LocalPort")>]
    let [<Literal>] localPort = "server.LocalPort"

    /// Was the request sent from the same machine? E.g. true or false.
    /// Type: Boolean
    [<CompiledName ("IsLocal")>]
    let [<Literal>] isLocal = "server.IsLocal"

    /// It is important for applications to be able to determine if a specific feature is supported by the current server or middleware.  The following pattern is recommended for announcing and detecting feature/extension support.
    /// Each extension SHOULD add to the capabilities IDictionary a "featurename.Version" key with the associated string value of the latest version of that extension supported (e.g. "1.2").
    [<CompiledName ("Capabilities")>]
    let [<Literal>] capabilities = "server.Capabilities"

    [<CompiledName ("ServerName")>]
    let [<Literal>] serverName = "server.Name"

    [<CompiledName ("OnSendingHeaders")>]
    let [<Literal>] onSendingHeaders = "server.OnSendingHeaders"

  /// http://owin.org/extensions/owin-SendFile-Extension-v0.3.0.htm
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

  /// http://owin.org/extensions/owin-OpaqueStream-Extension-v0.3.0.htm
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

  /// http://owin.org/extensions/owin-WebSocket-Extension-v0.4.0.htm
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
    let [<Literal>] receiveAsync = "websocket.ReceiveAsync
    "
    [<CompiledName ("CloseAsync")>]
    let [<Literal>] closeAsync = "websocket.CloseAsync"

    [<CompiledName ("CallCancelled")>]
    let [<Literal>] callCancelled = "websocket.CallCancelled"

    [<CompiledName ("ClientCloseStatus")>]
    let [<Literal>] clientCloseStatus = "websocket.ClientCloseStatus"

    [<CompiledName ("ClientCloseDescription")>]
    let [<Literal>] clientCloseDescription = "websocket.ClientCloseDescription"

  module MSFT =
    [<CompiledName "TraceFactoryDelegate">]
    let [<Literal>] traceFactoryDelegate = "server.LoggerFactory"

type OwinEnvironment =
  IDictionary<string, obj>

type OwinApp =
  OwinEnvironment -> Async<unit>

type OwinAppFunc =
  Func<OwinEnvironment, Task>

type OwinMidFunc =
  Func<Func<OwinEnvironment, Task>, Func<OwinEnvironment, Task>>

// TO CONSIDER: provide typed API in OwinRequest
type OwinRequest =
  /// Registers for an event that fires just before the response headers are sent.
  /// (Action<Action<obj>, obj>)
  abstract OnSendingHeaders<'State> : ('State -> unit) -> 'State -> unit

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinApp =

  open Suave.Utils.Aether
  open Suave.Utils.Aether.Operators
  open Suave.Utils
  open Suave.Sockets.Control
  open Suave.Web.ParsingAndControl
  open System.Text
  open System.Globalization
  open System.Diagnostics

  /// http://www.tugberkugurlu.com/archive/logging-in-the-owin-world-with-microsoft-owin--introduction
  type TraceFactoryDelegate =
    Func<string, Func<Diagnostics.TraceEventType, int, obj, Exception,
                      Func<obj, Exception, string>,
                      bool>>

  /// http://www.tugberkugurlu.com/archive/logging-in-the-owin-world-with-microsoft-owin--introduction
  let traceFactory (suaveLogger : Logger) : TraceFactoryDelegate =
    let createLogger name =
      new Func<TraceEventType, int, obj, exn, Func<obj, exn, string>, bool>(
        fun eventType eventId state ex formatter ->
          let exo = match ex with | null -> None | eee -> Some eee
          suaveLogger.Log LogLevel.Info (fun () ->
            LogLine.mk "Suave.Owin" LogLevel.Info TraceHeader.empty exo (formatter.Invoke (state, ex))
          )
          true
      )
    new Func<_, _>(createLogger)

  let textWriter (suaveLogger : Logger) : IO.TextWriter =
    { new IO.TextWriter(CultureInfo.InvariantCulture) with
        member x.Encoding = Encoding.UTF8
        override x.WriteLine (str : string) =
          suaveLogger.Log LogLevel.Info (fun () ->
            LogLine.mk "Suave.Owin" LogLevel.Info TraceHeader.empty None str
          )
    }

  type private Clock = uint64

  [<CustomEquality; CustomComparison>]
  type internal OwinKey =
    | OwinKey of string

    override x.GetHashCode() =
      let (OwinKey key) = x
      key.GetHashCode()

    member inline x.Equals(other : string) =
      let (OwinKey key) = x
      key.Equals(other, StringComparison.OrdinalIgnoreCase)

    member inline x.Equals(OwinKey other) =
      x.Equals(other)

    override x.Equals(obj) =
      match obj with
      | null -> false
      | :? OwinKey as other -> x.Equals(other)
      | :? string as other -> x.Equals(other)
      | _ -> invalidArg "obj" "The parameter obj must be of type string or OwinKey"

    member inline x.CompareTo(other : string) =
      let (OwinKey key) = x
      StringComparer.OrdinalIgnoreCase.Compare(key, other)

    member inline x.CompareTo(OwinKey other) =
      x.CompareTo(other)

    member x.CompareTo(obj : obj) =
      let (OwinKey key) = x
      match obj with
      | null -> 1
      | :? OwinKey as other -> x.CompareTo(other)
      | :? string as other -> x.CompareTo(other)
      | _ -> invalidArg "obj" "The parameter obj must be of type string or OwinKey"

    interface IComparable with
      member x.CompareTo(obj) = x.CompareTo(obj)

  // NOTE: a custom Map type might be more performant.
  type internal DeltaDictionary private (initialHeaders : Map<OwinKey, string[]>) =
    let changed : Map<OwinKey, Clock * string []> ref = ref Map.empty
    let removed : Map<OwinKey, Clock> ref = ref Map.empty
    let mutable clock = 1UL

    member x.Delta = // TO CONSIDER: memoize function keyed on logical clock
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

    member x.DeltaList =
      x.Delta |> Seq.map (fun (KeyValue(OwinKey key, value)) -> key, String.concat ", " value)
              |> Seq.toList

    new(dic : (string * string) list) =
      DeltaDictionary(dic |> List.map (fun (key, value) -> OwinKey key, [| value |]) |> Map.ofList)

    interface IDictionary<string, string[]> with
      member x.Item
        with get key =
          match !removed |> Map.tryFind (OwinKey key) with
          | Some rmCl ->
            match !changed |> Map.tryFind (OwinKey key) with
            | Some (chCl, value) ->
              if chCl > rmCl then value else raise (KeyNotFoundException())

            | None ->
              raise (KeyNotFoundException())

          | None ->
            !changed
            |> Map.tryFind (OwinKey key)
            |> Option.fold (fun s t -> snd t) (defaultArg (initialHeaders |> Map.tryFind (OwinKey key)) [||])

        and set key value =
          changed := !changed |> Map.put (OwinKey key) (clock, value)
          clock <- clock + 1UL

      member x.Remove key =
        let res =
          match !removed |> Map.tryFind (OwinKey key) with
          | Some rmCl ->
            match !changed |> Map.tryFind (OwinKey key) with
            | Some (chCl, value) ->
              if chCl > rmCl then
                removed := !removed |> Map.put (OwinKey key) clock
                true // changed after removed, so remove it again
              else
                false // removed after changed, nothing to do

            | None ->
              false // already removed, never changed

          | None ->
            match !changed |> Map.tryFind (OwinKey key) with
            | Some (chCl, value) ->
              removed := !removed |> Map.put (OwinKey key) clock
              true // remove after changed

            | None ->
              match initialHeaders |> Map.tryFind (OwinKey key) with
              | Some _ ->
                removed := !removed |> Map.put (OwinKey key) clock
                true // remove from initial
              | None ->
                false // never present

        clock <- clock + 1UL
        res

      member x.Keys = [| for KeyValue(OwinKey k,_) in x.Delta -> k |] :> ICollection<_>
      member x.Values = (x.Delta :> IDictionary<_, _>).Values
      member x.ContainsKey key = (x.Delta :> IDictionary<_, _>).ContainsKey(OwinKey key)
      member x.Add (key, value) = invalidOp "Add is not supported"
      member x.TryGetValue (key, [<Out>] res : byref<string[]>) = (x.Delta :> IDictionary<_, _>).TryGetValue(OwinKey key, ref res)

    interface ICollection<KeyValuePair<string, string[]>> with
      member x.Add kvp = invalidOp "Add is not supported"
      member x.Count = (x.Delta :> IDictionary<_, _>).Count
      member x.IsReadOnly = false
      member x.Clear() = invalidOp "Clear is not supported"
      member x.Contains (KeyValue(k, v)) = (x.Delta :> ICollection<_>).Contains(KeyValuePair(OwinKey k, v))
      member x.CopyTo (array, arrayIndex) = (x.Delta :> ICollection<_>).CopyTo([| for KeyValue(k, v) in array -> KeyValuePair(OwinKey k, v) |], arrayIndex)
      member x.Remove kvp = (x :> IDictionary<_, _>).Remove kvp.Key

    interface IEnumerable<KeyValuePair<string, string[]>> with
      member x.GetEnumerator() =
        let s = seq { for KeyValue(OwinKey k, v) in x.Delta -> KeyValuePair(k, v) }
        s.GetEnumerator()

    interface IEnumerable with
      member x.GetEnumerator() =
        let s = seq { for KeyValue(OwinKey k, v) in x.Delta -> KeyValuePair(k, v) }
        (s :> IEnumerable).GetEnumerator()

  module internal SirLensALot =

    let untyped<'t> : Iso<'t, obj> =
      (fun x ->
        box x),
      (fun x ->
        //failwithf "converting %s to %s" (x.GetType().Name) (typeof<'t>.Name)
        unbox x)

    let boundAddresses : Property<HttpContext, IList<IDictionary<string, obj>>> =
      (fun x ->
        let binding =
          Map [ "scheme", x.runtime.matchedBinding.scheme.ToString()
                "host", x.request.host
                "port", x.runtime.matchedBinding.socketBinding.port.ToString(CultureInfo.InvariantCulture)
                "path", x.runtime.matchedBinding.socketBinding.port.ToString(CultureInfo.InvariantCulture) ]
          |> Map.map (fun key value -> box value)
          :> IDictionary<string, obj>
        List<_> [ binding ] :> IList<_>
      ),
      fun v x -> invalidOp "cannot change the bound addresses after start."

    let methodString : Property<HttpMethod, string> =
      (fun x -> x.ToString()),
      (fun v x -> HttpMethod.parse v)

    let uriScheme : Property<_, string> =
      (fun (uri : Uri) -> uri.Scheme),
      (fun v uri -> UriBuilder(uri, Scheme = v).Uri)

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

    let bytesToStream : Property<byte[], IO.Stream> =
      (fun x -> upcast new IO.MemoryStream(x)),
      (fun v x ->
        use ms = new IO.MemoryStream()
        v.CopyTo ms // TO CONSIDER: save the stream instead and read from it when writing data
        ms.ToArray())

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

    let constant x =
      ((fun _ -> x),
       (fun v x -> x)
      ) <--> untyped

    let mapFindLens key : Property<Map<string, _>, _> =
      (fun x -> x |> Map.pick (fun k v -> if k.Equals(key, StringComparison.OrdinalIgnoreCase) then Some v else None)),
      (fun v x -> x |> Map.put key v)

    let stringlyTyped (toString : 'a -> string) (ofString : string -> 'a) : Iso<'a, string> =
      (fun x -> toString x),
      (fun v -> ofString v)

    let owinMap ct requestPathBase
                requestHeadersLens responseHeadersLens
                responseStreamLens onSendingHeadersLens =

      [ // 3.2.1 Request Data
        // writeable / value???
        OwinKey OwinConstants.requestScheme,        HttpContext.request_ >--> HttpRequest.url_ >--> uriScheme <--> untyped
        // writeable / value
        OwinKey OwinConstants.requestMethod,        HttpContext.request_ >--> HttpRequest.method_ >--> methodString <--> untyped
        // writeable / value
        OwinKey OwinConstants.requestPathBase,      constant requestPathBase
        // writeable / value
        OwinKey OwinConstants.requestPath,          HttpContext.request_ >--> HttpRequest.url_ >--> uriAbsolutePath <--> untyped
        // writeable / value
        OwinKey OwinConstants.requestQueryString,   HttpContext.request_ >--> HttpRequest.rawQuery_ <--> untyped
        // writeable / value???
        OwinKey OwinConstants.requestProtocol,      HttpContext.request_ >--> HttpRequest.httpVersion_ >--> hv2p <--> untyped
        // !! mutation expected (!)
        OwinKey OwinConstants.requestHeaders,       HttpContext.request_ >--> HttpRequest.headers_ >--> requestHeadersLens <--> untyped
        // writeable / value
        OwinKey OwinConstants.requestBody,          HttpContext.request_ >--> HttpRequest.rawForm_ >--> bytesToStream <--> untyped
        OwinKey OwinConstants.requestId,            HttpContext.request_ >--> HttpRequest.trace_ >--> TraceHeader.traceId_ <--> stringlyTyped string uint64 <--> untyped
        // writeable / value
        OwinKey OwinConstants.requestUser,          HttpContext.userState_ >--> claimsPrincipal <--> untyped

        // 3.2.2 Response Data
        // writeable / value
        OwinKey OwinConstants.responseStatusCode,   HttpContext.response_ >--> HttpResult.status_ >--> i2sc <--> untyped
        // TO CONSIDER: add support for modifying phrasing to Core?
        // writeable / value
        OwinKey OwinConstants.responseReasonPhrase, constant "Changing the reason phrase is not supported in Suave"
        // writeable / value???
        OwinKey OwinConstants.responseProtocol,     HttpContext.request_ >--> HttpRequest.httpVersion_ >--> hv2p <--> untyped
        // !! mutation expected
        OwinKey OwinConstants.responseHeaders,      HttpContext.response_ >--> HttpResult.headers_ >--> responseHeadersLens <--> untyped
        // !! mutation expected
        OwinKey OwinConstants.responseBody,         HttpContext.response_ >--> HttpResult.content_ >--> responseStreamLens <--> untyped

        // 3.2.3 Other Data
        OwinKey OwinConstants.callCancelled,        constant ct // TODO: support cancellation token in HttpRequest signalling aborted request
        OwinKey OwinConstants.owinVersion,          constant "1.3"

        // Common Keys
        OwinKey OwinConstants.CommonKeys.addresses,         boundAddresses <--> untyped
        OwinKey OwinConstants.CommonKeys.serverName,        constant "Suave"
        OwinKey OwinConstants.CommonKeys.capabilities,      constant (
          Map [
            OwinKey "owin.Version", "1.0.1"
            OwinKey "suave.Version", Globals.Internals.SuaveVersion
          ]
          |> Map.map (fun key value -> box value)
          :> IDictionary<OwinKey, obj>
        )
        OwinKey OwinConstants.CommonKeys.clientCertificate, constant Unchecked.defaultof<Security.Cryptography.X509Certificates.X509Certificate>
        OwinKey OwinConstants.CommonKeys.onSendingHeaders,  onSendingHeadersLens <--> untyped
        OwinKey OwinConstants.CommonKeys.isLocal,           HttpContext.isLocal_ <--> untyped
        OwinKey OwinConstants.CommonKeys.localIpAddress,    HttpContext.runtime_ >--> HttpRuntime.matchedBinding_ >--> HttpBinding.socketBinding_ >--> SocketBinding.ip_ <--> stringlyTyped (sprintf "%O") IPAddress.Parse <--> untyped
        OwinKey OwinConstants.CommonKeys.localPort,         HttpContext.runtime_  >--> HttpRuntime.matchedBinding_ >--> HttpBinding.socketBinding_ >--> SocketBinding.port_ <--> stringlyTyped string uint16 <--> untyped
        OwinKey OwinConstants.CommonKeys.remoteIpAddress,   HttpContext.clientIp_ <--> stringlyTyped (sprintf "%O") IPAddress.Parse <--> untyped
        OwinKey OwinConstants.CommonKeys.remotePort,        HttpContext.clientPort_ <--> stringlyTyped string uint16 <--> untyped
        OwinKey OwinConstants.CommonKeys.traceOutput,       HttpContext.runtime_ >--> HttpRuntime.logger_ >--> ((fun x -> textWriter x), (fun v x -> x)) <--> untyped
        
        // per-request storage
        OwinKey "suave.UserData",                           HttpContext.userState_ <--> untyped

        // MSFT non standard
        // readable
        OwinKey OwinConstants.MSFT.traceFactoryDelegate,    HttpContext.runtime_ >--> HttpRuntime.logger_ >--> ((fun x -> traceFactory x), (fun v x -> x)) <--> untyped
      ]

  type internal OwinDictionary(requestPathBase, initialState) =
    // TODO: support streaming optionally
      //let impl conn = AsyncSocket.transferStream conn v
      //SocketTask impl
    // TODO: cancelled request

    let cts = new CancellationTokenSource()
    let state : HttpContext ref = ref initialState //externally owned

    // NOTE: I cannot make OwinDictionary immutable, because the Task OWIN returns doesn't have
    // a proper return value (only unit)
    //
    // the first time any of these are requested, set the ref, return the reference
    let requestHeaders : DeltaDictionary option ref = ref None
    let requestHeadersLens : Property<(string * string) list, IDictionary<string, string[]>> =
      (fun x ->
        upcast (match !requestHeaders with
                | None   -> let v = DeltaDictionary x in requestHeaders := Some v ; v
                | Some v -> v)),
      (fun v x ->
        match !requestHeaders with
        | Some v' when Object.ReferenceEquals (v, v') -> x
        | _ -> invalidOp "setting RequestHeaders IDictionary<string, string[]> is not supported")

    let responseHeaders : DeltaDictionary option ref = ref None
    let responseHeadersLens : Property<(string * string) list, IDictionary<string, string[]>> =
      (fun x ->
        upcast (match !responseHeaders with
                | None   -> let v = DeltaDictionary x in responseHeaders := Some v ; v
                | Some v -> v)),
      (fun v x ->
        match !responseHeaders with
        | Some v' when Object.ReferenceEquals (v, v') -> x
        | _ -> invalidOp "setting ResponseHeaders IDictionary<string, string[]> is not supported")

    let responseStream = new OpenMemoryStream()
    let responseStreamLens : Property<HttpContent, IO.Stream> =
      (fun x ->
        let bs = Lens.getPartialOrElse HttpContent.BytesPLens [||] x
        responseStream.Write(bs, 0, bs.Length)
        upcast responseStream),
      (fun v x ->
        match responseStream with
        | v' when Object.ReferenceEquals (v, v') -> x
        | _ -> invalidOp "setting responseStream to a value is not supported in OWIN")

    let sendingHeaders : ((obj -> unit) * obj) list ref = ref []
    let onSendingHeadersLens : Property<HttpContext, Action<Action<obj>, obj>> =
      (fun x ->
        Action<_, _>(fun (cb : Action<obj>) (st : obj) ->
          sendingHeaders := (cb.Invoke, st) :: !sendingHeaders
          ())),
      (fun v x ->
        match !sendingHeaders with
        | v'  when Object.ReferenceEquals (v, v') -> x
        | _ -> invalidOp "cannot set onSendingHeadersAction")

    let owinMap = SirLensALot.owinMap cts.Token requestPathBase
                                      requestHeadersLens responseHeadersLens
                                      responseStreamLens onSendingHeadersLens
    let owinKeys = owinMap |> List.map fst |> Set.ofSeq
    let owinRW   = owinMap |> Map.ofList

    let owinLensLens key : Lens<Map<OwinKey, Property<HttpContext, obj>>, Property<HttpContext, obj>> =
      let userDataItem_ = HttpContext.userState_ >--> SirLensALot.mapFindLens key <--> SirLensALot.untyped
      (fun x ->
        x
        |> Map.tryPick (fun k v -> if k.Equals(key) then Some v else None)
        |> function | None -> userDataItem_
                    | Some lens -> lens),
      (fun v x -> invalidOp "setting owin constants not supported")

    interface OwinRequest with
      member x.OnSendingHeaders cb st =
        sendingHeaders := ((fun o -> cb (unbox o)), box st) :: !sendingHeaders

    member x.Interface =
      x :> OwinEnvironment

    /// Calling this returns a valid HttpResult that we can use for Suave
    member x.Finalise() =
      let reqHeaders_, respHeaders_, bytes_ =
        HttpContext.request_ >--> HttpRequest.headers_,
        HttpContext.response_ >--> HttpResult.headers_,
        HttpContext.response_ >--> HttpResult.content_ >-?> HttpContent.BytesPLens

      let setResponseHeaders (rhs : DeltaDictionary) =
        Lens.set respHeaders_ rhs.DeltaList

      let setRequestHeaders (rhs : DeltaDictionary) =
        Lens.set reqHeaders_ rhs.DeltaList

      let setResponseData (ms : OpenMemoryStream) =
        ms.Seek(0L, IO.SeekOrigin.Begin) |> ignore
        Lens.setPartial bytes_ (ms.ToArray())

      List.foldBack (fun (cb, st) s -> cb st) !sendingHeaders ()

      !state
      |> Option.foldBack setRequestHeaders !requestHeaders
      |> Option.foldBack setResponseHeaders !responseHeaders
      |> setResponseData responseStream

    interface IDictionary<string, obj> with
      member x.Remove k =
        invalidOp "Remove not supported"

      member x.Item
        with get key =
          Lens.get (Lens.get (owinLensLens key) owinRW) !state
        and set key value =
          let settable = Lens.get (owinLensLens key) owinRW
          state := Lens.set settable value !state

      member x.Keys =
        owinKeys |> Set.map (fun (OwinKey key) -> key) :> ICollection<_>

      member x.Values =
        (owinRW |> Map.map (fun key valueLens -> Lens.get valueLens !state) :> IDictionary<_, _>).Values

      member x.ContainsKey k =
        owinKeys.Contains(OwinKey k)

      member x.Add (key, v) =
        (x :> IDictionary<_, _>).[key] <- v

      member x.TryGetValue (key, [<Out>] res : byref<obj>) =
        if owinKeys |> Set.contains (OwinKey key) then
          res <- (x :> IDictionary<_, _>).[key]
          true
        else
          false

    interface ICollection<KeyValuePair<string, obj>> with
      member x.Add kvp = (x :> IDictionary<_, _>).Add(kvp.Key, kvp.Value)
      member x.Count = owinKeys.Count
      member x.IsReadOnly = false
      member x.Clear() = invalidOp "Clear is not supported"
      member x.Contains kvp = owinKeys.Contains(OwinKey kvp.Key)
      member x.CopyTo (array, arrayIndex) = invalidOp "CopyTo is not supported"
      member x.Remove kvp = (x :> IDictionary<_, _>).Remove kvp.Key

    interface IEnumerable<KeyValuePair<string, obj>> with
      member x.GetEnumerator() =
        (x :> IDictionary<_, _>).Keys
        |> Seq.map (fun key -> KeyValuePair(key, (x :> IDictionary<_, _>).[key]))
        |> fun seq -> seq.GetEnumerator()

    interface IEnumerable with
      member x.GetEnumerator() =
        (x :> IDictionary<_, _>).Keys
        |> Seq.map (fun key -> KeyValuePair(key, (x :> IDictionary<_, _>).[key]))
        |> fun seq -> (seq :> IEnumerable).GetEnumerator()

    interface IDisposable with
      member x.Dispose() =
        responseStream.RealDispose()
        (cts :> IDisposable).Dispose()

  [<CompiledName "OfApp">]
  let ofApp (requestPathBase : string) (owin : OwinApp) : WebPart =

    Applicatives.pathStarts requestPathBase >>=
    fun (ctx : HttpContext) ->
      let verbose f =
        ctx.runtime.logger.Log LogLevel.Verbose (
          f >> LogLine.mk "Suave.Owin" LogLevel.Verbose ctx.request.trace None
        )

      let impl (conn, response) : SocketOp<unit> = socket {
        let owinRequestUri = UriBuilder ctx.request.url

        owinRequestUri.Path <-
          if ctx.request.url.AbsolutePath.StartsWith requestPathBase then
            ctx.request.url.AbsolutePath.Substring requestPathBase.Length
          else
            ctx.request.url.AbsolutePath

        // disposable because it buffers what the OwinApp writes to the stream
        // also, OWIN expects the default HTTP status code to be 200 OK
        // could also set the status code after calling SocketOp.ofAsync if the value is not set
        use wrapper =
          new OwinDictionary(
            requestPathBase,
            { ctx with
                request = { ctx.request with url = owinRequestUri.Uri }
                response = { response with status = HTTP_200 } })

        verbose (fun _ -> "yielding to OWIN middleware")
        do! SocketOp.ofAsync (owin wrapper.Interface)

        verbose (fun _ -> "suave back in control")
        let ctx = wrapper.Finalise()

        verbose (fun _ -> "writing preamble")
        do! Web.ParsingAndControl.writePreamble ctx

        verbose (fun _ -> "writing body")
        do! Web.ParsingAndControl.writeContent ctx ctx.response.content
      }

      { ctx with
          response =
            { ctx.response with
                content       = SocketTask impl
                writePreamble = false
            }
      }
      |> succeed

  [<CompiledName "OfAppFunc">]
  let ofAppFunc requestPathBase (owin : OwinAppFunc) =
    ofApp requestPathBase
          (fun e -> Async.AwaitTask ((owin.Invoke e).ContinueWith<_>(fun x -> ())))

  [<CompiledName "OfMidFunc">]
  let ofMidFunc requestPathBase (owin : OwinMidFunc) =
    ofAppFunc requestPathBase
              (owin.Invoke(fun _ -> new Task(fun x -> ())))

module OwinServerFactory =

  type Dic<'Key, 'Value> = IDictionary<'Key, 'Value>

  let private read (d : OwinEnvironment) k f =
    match d.TryGetValue k with
    | false, _ -> f ()
    | true, value -> value :?> 'a

  let private readEnv (e : OwinEnvironment) key =
    let res = read e key (fun () -> new Dictionary<string, obj>() :> OwinEnvironment)
    e.[key] <- res
    res

  let private readDic (e : OwinEnvironment) key =
    let res = read e key (fun () -> new Dictionary<string, 'a>())
    e.[key] <- res
    res

  let private readList (e : OwinEnvironment) key =
    let res = read e key (fun () -> new List<'a>() :> IList<'a>)
    e.[key] <- res
    res

  let private get (e : OwinEnvironment) key =
    match e.TryGetValue key with
    | false, _ -> failwithf "missing value for key '%s'" key
    | true, value -> value :?> 'a

  let private getDefault (e : OwinEnvironment) key map defaults =
    match e.TryGetValue key with
    | false, _ -> defaults
    | true, value -> map (value :?> 'a)

  [<CompiledName "Initialize">]
  let initialize (props : Dic<string, obj>) =
    if props = null then nullArg "props"
    props.[OwinConstants.owinVersion] <- "1.0.1"
    let cap = readEnv props OwinConstants.CommonKeys.capabilities
    cap.[OwinConstants.CommonKeys.serverName] <- "Suave"

  [<CompiledName ("Create")>]
  let create (app : OwinAppFunc, props : Dic<string, obj>) =
    if app = null then nullArg "app"
    if props = null then nullArg "props"

    let bindings =
      (readList props OwinConstants.CommonKeys.addresses : IList<OwinEnvironment>)
      |> Seq.map (fun dic ->
        let port   = get dic "port" : string
        let ip     = read dic "ip" (fun () -> "127.0.0.1") : string
        let scheme = getDefault dic "certificate" (fun _ -> HTTP) HTTP
        { scheme = scheme; socketBinding = { ip = IPAddress.Parse ip; port = uint16 port } })
      |> List.ofSeq

    let serverCts = new CancellationTokenSource()

    let conf =
      { Web.defaultConfig with
          bindings          = bindings
          cancellationToken = serverCts.Token }

    let started, listening =
      Web.startWebServerAsync conf (OwinApp.ofAppFunc "/" app)

    listening |> Async.Start
    let _ = started |> Async.RunSynchronously

    { new IDisposable with
        member x.Dispose () =
          // note: this won't let the web requests finish gently
          serverCts.Cancel()
          serverCts.Dispose()
      }
