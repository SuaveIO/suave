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

module Aether =

  (* Types

     Types defining lenses and isomorphisms (both total
     and partial as standard pairs. These can be implemented implicitly,
     so an assembly *providing* lenses without also consuming them
     requires no dependency on Aether, just an implicit structuring. *)

  /// Total lens from a -> b
  type Lens<'a,'b> = ('a -> 'b) * ('b -> 'a -> 'a)

  /// Partial lens from a -> b
  type PLens<'a,'b> = ('a -> 'b option) * ('b -> 'a -> 'a)

  // Isomorphisms

  /// Total isomorphism of a <> b
  type Iso<'a,'b> = ('a -> 'b) * ('b -> 'a)

  /// Partial isomorphism of a <> b
  type PIso<'a,'b> = ('a -> 'b option) * ('b -> 'a)

  (* Functions

     Functions for using lenses to get, set and modify values within a target
     instance. *)

  [<RequireQualifiedAccess>]
  module Lens =

      /// Get a value using a total lens
      let get ((g, _): Lens<'a,'b>) = 
          fun a -> g a

      /// Get a value option using a partial lens
      let getPartial ((g, _): PLens<'a,'b>) = 
          fun a -> g a

      /// Get a value or a default using a partial lens
      let getPartialOrElse ((g, _): PLens<'a,'b>) = 
          fun b a -> g a |> function | Some b -> b | _ -> b

      /// Set a value using a total lens
      let set ((_, s): Lens<'a,'b>) =
          fun b a -> s b a

      /// Set a value using a partial lens
      let setPartial ((_, s): PLens<'a,'b>) =
          fun b a -> s b a

      /// Modify a value using a total lens
      let map ((g, s): Lens<'a,'b>) = 
          fun f a -> s (f (g a)) a

      /// Modify a value using a partial lens
      let mapPartial ((g, s): PLens<'a,'b>) = 
          fun f a -> Option.map f (g a) |> function | Some b -> s b a | _ -> a

  (* Compositions

     Functions for composing lenses and isomorphisms, each of which
     returns a new lens of a total or partial type based on the lenses
     or isomorphisms composed. It is more common (and significantly less
     verbose) to use the infix operator forms of these compositions (though note
     that Aether.Operators is not open by default and should be opened explicitly). *)

  [<RequireQualifiedAccess>]
  module Compose =

      /// Compose a total lens and a total lens, giving a total lens
      let totalLensTotalLens ((g1, s1): Lens<'a,'b>) ((g2, s2): Lens<'b,'c>) : Lens<'a,'c> =
          (fun a -> g2 (g1 a)),
          (fun c a -> s1 (s2 c (g1 a)) a)

      /// Compose a total lens and a partial lens, giving a partial lens
      let totalLensPartialLens ((g1, s1): Lens<'a,'b>) ((g2, s2): PLens<'b,'c>) : PLens<'a,'c> =
          (fun a -> g2 (g1 a)),
          (fun c a -> s1 (s2 c (g1 a)) a)

      /// Compose a partial lens and a total lens, giving a partial lens
      let partialLensTotalLens ((g1, s1): PLens<'a,'b>) ((g2, s2): Lens<'b,'c>) : PLens<'a,'c> =
          (fun a -> Option.map g2 (g1 a)),
          (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a | _ -> a)

      /// Compose two partial lenses, giving a partial lens
      let partialLensPartialLens ((g1, s1): PLens<'a,'b>) ((g2, s2): PLens<'b,'c>) : PLens<'a,'c> =
          (fun a -> Option.bind g2 (g1 a)),
          (fun c a -> Option.map (s2 c) (g1 a) |> function | Some b -> s1 b a | _ -> a)

      /// Compose a total lens with a total isomorphism, giving a total lens
      let totalLensTotalIsomorphism ((g, s): Lens<'a,'b>) ((f, t): Iso<'b,'c>) : Lens<'a,'c> =
          (fun a -> f (g a)),
          (fun c a -> s (t c) a)

      /// Compose a total lens with a partial isomorphism, giving a partial lens
      let totalLensPartialIsomorphism ((g, s): Lens<'a,'b>) ((f, t): PIso<'b,'c>) : PLens<'a,'c> =
          (fun a -> f (g a)),
          (fun c a -> s (t c) a)

      /// Compose a partial lens with a total isomorphism, giving a partial lens
      let partialLensTotalIsomorphism ((g, s): PLens<'a,'b>) ((f, t): Iso<'b, 'c>) : PLens<'a,'c> =
          (fun a -> Option.map f (g a)),
          (fun c a -> s (t c) a)

      /// Compose a partial lens with a partial isomorphism, giving a partial lens
      let partialLensPartialIsomorphism ((g, s): PLens<'a,'b>) ((f, t): PIso<'b,'c>) : PLens<'a,'c> =
          (fun a -> Option.bind f (g a)),
          (fun c a -> s (t c) a)

  (* Lenses

     Various lenses implemented for common types such as tuples,
     lists and maps, along with an id lens (which is useful for composing
     a lens which has not specific "lensing" elements but is implicitly a chain
     of one or more isomorphisms. Having an id lens enables the root composition. *)

  /// Identity lens returning the original item regardless of modifiction
  let idLens : Lens<'a,'a> =
      (fun x -> x), (fun x _ -> x) 

  /// First item of a tuple giving a total lens
  let fstLens : Lens<('a * 'b),'a> =
      fst, (fun a t -> a, snd t)
          
  /// Second item of a tuple giving a total lens
  let sndLens : Lens<('a * 'b),'b> =
      snd, (fun b t -> fst t, b)

  /// Head of a list giving a partial lens
  let headPLens : PLens<'v list, 'v> =
      (function | h :: _ -> Some h | _ -> None),
      (fun v -> function | _ :: t -> v :: t | l -> l)

  /// Position of a list giving a partial lens
  let listPLens (i: int) : PLens<'v list, 'v> =
      (function | l when List.length l > i -> Some (List.nth l i) | _ -> None), 
      (fun v l -> List.mapi (fun i' x -> if i = i' then v else x) l)

  /// Tail of a list giving a partial lens
  let tailPLens : PLens<'v list, 'v list> =
      (function | _ :: t -> Some t | _ -> None),
      (fun t -> function | h :: _ -> h :: t | [] -> t)

  /// Key of a map giving a partial lens
  let mapPLens (k: 'k) : PLens<Map<'k,'v>,'v> =
      Map.tryFind k, Map.add k

  (* Operators

     Operators are an optional feature of Aether and so must be explicitly opened
     when needed. *)

  module Operators =

      (* Composition Operators

         Operators as syntactical alternatives to more verbose composition
         functions given. These are expected to be much more commonly used
         and syntactially provide more clues as to their function. *)

      /// Compose a total lens and a total lens, giving a total lens
      let (>-->) l1 l2 =
          Compose.totalLensTotalLens l1 l2

      /// Compose a total lens and a partial lens, giving a partial lens
      let (>-?>) l1 l2 =
          Compose.totalLensPartialLens l1 l2

      /// Compose a partial lens and a total lens, giving a partial lens
      let (>?->) l1 l2 =
          Compose.partialLensTotalLens l1 l2

      /// Compose two partial lenses, giving a partial lens
      let (>??>) l1 l2 =
          Compose.partialLensPartialLens l1 l2

      /// Compose a total lens with a total isomorphism, giving a total lens
      let (<-->) l i =
          Compose.totalLensTotalIsomorphism l i

      /// Compose a total lens with a partial isomorphism, giving a partial lens
      let (<-?>) l i =
          Compose.totalLensPartialIsomorphism l i

      /// Compose a partial lens with a total isomorphism, giving a partial lens
      let (<?->) l i =
          Compose.partialLensTotalIsomorphism l i

      /// Compose a partial lens with a partial isomorphism, giving a partial lens
      let (<??>) l i =
          Compose.partialLensPartialIsomorphism l i

      (* Function Operators

         Operators as infix alternatives to some of the standard get, set,
         modify functions (getL, setL, etc.) Should likely be used rather 
         sparingly and in specific controlled areas unless you're aiming for 
         symbol soup. *)

      /// Get a value using a total lens
      let (^.) (a: 'a) (l: Lens<'a,'b>) : 'b =
          Lens.get l a

      /// Get a value using a partial lens
      let (^?.) (a: 'a) (l: PLens<'a,'b>) : 'b option =
          Lens.getPartial l a

      /// Set a value using a total lens
      let (^=) (b: 'b) (l: Lens<'a,'b>) : 'a -> 'a =
          Lens.set l b

      /// Set a value using a partial lens
      let (^?=) (b: 'b) (l: PLens<'a,'b>) : 'a -> 'a =
          Lens.setPartial l b

      /// Modify a value using a total lens
      let (^%=) (f: 'b -> 'b) (l: Lens<'a,'b>) : 'a -> 'a =
          Lens.map l f

      /// Modify a value using a partial lens
      let (^?%=) (f: 'b -> 'b) (l: PLens<'a,'b>) : 'a -> 'a =
          Lens.mapPartial l f

[<RequireQualifiedAccess>]
[<CompilationRepresentation (CompilationRepresentationFlags.ModuleSuffix)>]
module OwinAppFunc =

  open Aether
  open Aether.Operators
  open Suave.Utils
  open Suave.Sockets.Control
  open Suave.Web.ParsingAndControl

  type OwinRequest =
    abstract OnSendingHeadersAction : Action<Action<obj>, obj>

  let isoBox<'t> : Iso<'t, obj> =
    box, unbox

  type internal DWr(initialState) =
    let state : HttpContext ref = ref initialState

    let req l =
      HttpContext.request_ >--> l <--> isoBox

    let run l =
      HttpContext.runtime_ >--> l <--> isoBox

    let uriAbsolutePath : Property<_, _> =
      (fun (uri : Uri) -> uri.AbsolutePath),
      (fun v uri -> UriBuilder(uri, Path = v).Uri)

    let hv2p : Property<string, string> =
      (function  | "2.0" -> "HTTP/2.0"
                 | "1.0" -> "HTTP/1.0"
                 | _ | "1.1" -> "HTTP/1.1"),
      (fun v -> function | "HTTP/2.0" -> "2.0"
                         | "HTTP/1.0" -> "1.0"
                         | "HTTP/1.1" -> "1.1"
                         | x -> x)

    let owinMap =
      [ (* 3.2.1 Request Data *)
        OwinConstants.requestScheme, req HttpRequest.httpVersion_
        OwinConstants.requestMethod, req HttpRequest.method_
        OwinConstants.requestPathBase, run HttpRuntime.homeDirectory_
        OwinConstants.requestPath, HttpContext.request_ >--> HttpRequest.url_ >--> uriAbsolutePath <--> isoBox
        OwinConstants.requestQueryString, req HttpRequest.rawQuery_
        OwinConstants.requestProtocol, HttpContext.request_ >--> HttpRequest.httpVersion_ >--> hv2p <--> isoBox
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
