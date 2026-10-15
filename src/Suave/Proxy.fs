/// Forward requests to another HTTP server (a reverse proxy).
///
/// The method, raw path and query string, request body and end-to-end headers
/// are forwarded; hop-by-hop headers (RFC 9110 §7.6.1) are not. The upstream's
/// status, reason phrase, headers and body are streamed back to the client.
///
/// Limitation: Suave parses `multipart/form-data` bodies before any WebPart
/// runs (into `request.files` and `request.multiPartFields`), so their raw bytes
/// are not available and such requests reach the upstream without a body.
/// Other bodies, including `application/x-www-form-urlencoded`, are forwarded.
module Suave.Proxy

open System
open System.Collections.Generic
open System.Net
open System.Net.Http
open System.Text
open System.Threading
open System.Threading.Tasks
open Suave
open Suave.Sockets

/// How `proxyWith` forwards requests. Start from `defaultProxyOptions`.
type ProxyOptions =
  { /// How long to wait for the upstream's response headers before answering
    /// 504 Gateway Timeout. The body is then streamed without a time limit.
    timeout : TimeSpan
    /// Addresses of reverse proxies in front of this server whose
    /// X-Forwarded-For/-Proto/-Host and Forwarded headers are trusted: their
    /// X-Forwarded-For chain is extended and their -Proto/-Host are kept. From
    /// any other client these headers are replaced (or, for Forwarded and
    /// X-Real-IP, dropped), so a client cannot claim to have used HTTPS.
    trustedProxies : IPAddress list
    /// Request headers never forwarded, in addition to the hop-by-hop ones.
    dropRequestHeaders : string list
    /// Called when forwarding fails, before the 502 or 504 is sent. Log here.
    onError : HttpContext -> exn -> unit }

/// A 30 second timeout, no trusted proxies, nothing extra dropped, errors ignored.
let defaultProxyOptions =
  { timeout = TimeSpan.FromSeconds 30.
    trustedProxies = []
    dropRequestHeaders = []
    onError = fun _ _ -> () }

let private comparer = StringComparer.OrdinalIgnoreCase

let private set (names : string list) = HashSet<string>(names, comparer)

/// Headers that describe one connection, not the message (RFC 9110 §7.6.1).
let private hopByHop =
  set [ "connection"; "keep-alive"; "proxy-connection"; "te"; "trailer"; "transfer-encoding"; "upgrade"
        "proxy-authorization"; "proxy-authenticate" ]

/// Request headers the proxy sets itself, or that only make sense on the
/// client's connection (Suave has already answered any `Expect: 100-continue`).
let private replacedOnRequest = set [ "host"; "content-length"; "expect" ]

/// Request headers describing earlier hops, only kept from trusted proxies.
let private forwardingHeaders =
  set [ "x-forwarded-for"; "x-forwarded-proto"; "x-forwarded-host"; "forwarded"; "x-real-ip" ]

/// Response headers Suave writes itself.
let private replacedOnResponse = set [ "date"; "server"; "content-length" ]

/// Headers named in a Connection header are hop-by-hop too.
let private namedInConnection (headers : seq<string * string>) =
  headers
  |> Seq.filter (fun (name, _) -> comparer.Equals(name, "connection"))
  |> Seq.collect (fun (_, value) -> value.Split(',', StringSplitOptions.TrimEntries ||| StringSplitOptions.RemoveEmptyEntries))
  |> fun names -> HashSet<string>(names, comparer)

let private normalizeIp (ip : IPAddress) =
  if ip.IsIPv4MappedToIPv6 then ip.MapToIPv4() else ip

/// A path segment of `.` or `..` (possibly percent-encoded) would be removed
/// by `Uri`, letting `/../x` escape the upstream's base path.
let private hasDotSegment (rawPath : string) =
  rawPath.Split('/')
  |> Array.exists (fun segment ->
      match Uri.UnescapeDataString segment with
      | "." | ".." -> true
      | _ -> false)

let private targetUri (upstream : Uri) (request : HttpRequest) =
  let basePath = upstream.AbsolutePath.TrimEnd '/'
  let query = if String.IsNullOrEmpty request.rawQuery then "" else "?" + request.rawQuery
  Uri(upstream.GetLeftPart(UriPartial.Authority) + basePath + request.rawPath + query)

let private buildRequest (options : ProxyOptions) (trusted : HashSet<IPAddress>) (upstream : Uri) (ctx : HttpContext) =
  let request = ctx.request
  let message = new HttpRequestMessage(HttpMethod(request.rawMethod), targetUri upstream request)
  let remote = normalizeIp (ctx.clientIp false [])
  let fromTrustedProxy = trusted.Contains remote
  let dropped = namedInConnection request.headers
  let hasBody = request.rawForm.Length > 0
  if hasBody then message.Content <- new ByteArrayContent(request.rawForm)
  for (name, value) in request.headers do
    let skip =
      hopByHop.Contains name || replacedOnRequest.Contains name || dropped.Contains name
      || (forwardingHeaders.Contains name && not fromTrustedProxy)
      || comparer.Equals(name, "x-forwarded-for")
      || options.dropRequestHeaders |> List.exists (fun d -> comparer.Equals(d, name))
    // Content headers (Content-Type, Content-Encoding, ...) are rejected by
    // the message's headers and belong on its content.
    if not skip && not (message.Headers.TryAddWithoutValidation(name, value)) && hasBody then
      message.Content.Headers.TryAddWithoutValidation(name, value) |> ignore

  // X-Forwarded-For is a chain: extend a trusted proxy's, otherwise start one.
  let forwardedFor =
    let existing =
      request.headers
      |> Seq.filter (fun (name, value) -> comparer.Equals(name, "x-forwarded-for") && not (String.IsNullOrWhiteSpace value))
      |> Seq.map snd
      |> List.ofSeq
    if fromTrustedProxy && not existing.IsEmpty then
      String.Join(", ", existing @ [ string remote ])
    else
      string remote
  message.Headers.TryAddWithoutValidation("X-Forwarded-For", forwardedFor) |> ignore
  // A trusted proxy's -Proto and -Host were forwarded above.
  if not (fromTrustedProxy && message.Headers.Contains "X-Forwarded-Proto") then
    let proto = if request.binding.scheme.secure then "https" else "http"
    message.Headers.TryAddWithoutValidation("X-Forwarded-Proto", proto) |> ignore
  if not (fromTrustedProxy && message.Headers.Contains "X-Forwarded-Host") then
    message.Headers.TryAddWithoutValidation("X-Forwarded-Host", request.rawHost) |> ignore
  message

let private httpResponseToHttpContext (ctx : HttpContext) (response : HttpResponseMessage) =
  let code = int response.StatusCode
  let reason =
    match response.ReasonPhrase with
    | null | "" ->
      match HttpCode.tryParse code with
      | Choice1Of2 c -> c.reason
      | Choice2Of2 _ -> ""
    | phrase -> phrase

  // `HttpClient` splits response headers between the message and its content.
  // Keep every value separately so repeated headers such as Set-Cookie survive.
  let all =
    [ for h in response.Headers do for v in h.Value -> h.Key, v
      for h in response.Content.Headers do for v in h.Value -> h.Key, v ]
  let dropped = namedInConnection all
  let headers =
    all |> List.filter (fun (name, _) ->
      not (hopByHop.Contains name || replacedOnResponse.Contains name || dropped.Contains name))

  // Reading the content stream de-chunks the upstream body, so a body without
  // a known length is re-chunked on the way out.
  let length = response.Content.Headers.ContentLength
  let bodyless = ctx.request.``method`` = HttpMethod.HEAD || code = 204 || code = 304 || code < 200
  let chunked = not length.HasValue && not bodyless

  let content =
    SocketTask
      (fun (conn, _) -> task {
          use response = response
          if length.HasValue then
            do! conn.asyncWriteLn $"Content-Length: {length.Value}"
          elif chunked then
            do! conn.asyncWriteLn "Transfer-Encoding: chunked"
          do! conn.asyncWriteLn ""
          do! conn.flush()
          if not bodyless then
            use! stream = response.Content.ReadAsStreamAsync()
            if chunked then
              do! transferStreamChunked conn stream
            else
              do! transferStream conn stream
       })

  { ctx with
      response =
        { ctx.response with
            status = { code = code; reason = reason }
            headers = headers
            content = content } }

let private failWith (status : HttpCode) (message : string) (ctx : HttpContext) =
  Response.response status (Encoding.UTF8.GetBytes message) ctx

/// One `HttpClient` for all proxies: creating one per request exhausts sockets
/// under load. Cookies are off so one client's cookies are never sent with
/// another's request, and pooled connections are recycled so DNS changes are
/// picked up. Timeouts are per request, see `ProxyOptions.timeout`.
let private client =
  let handler =
    new SocketsHttpHandler(
      AllowAutoRedirect = false,
      UseCookies = false,
      UseProxy = false,
      PooledConnectionLifetime = TimeSpan.FromMinutes 2.)
  new HttpClient(handler, Timeout = Timeout.InfiniteTimeSpan)

/// Forwards requests to `upstream`, whose path, if any, prefixes every
/// forwarded path (`http://backend/api` maps `/users?id=1` to
/// `http://backend/api/users?id=1`). Answers 502 when the upstream cannot be
/// reached and 504 when it does not answer within `options.timeout`. Throws
/// `ArgumentException` immediately for a non-http(s) upstream or a
/// non-positive timeout.
let proxyWith (options : ProxyOptions) (upstream : Uri) : WebPart =
  if not upstream.IsAbsoluteUri || (upstream.Scheme <> Uri.UriSchemeHttp && upstream.Scheme <> Uri.UriSchemeHttps) then
    invalidArg (nameof upstream) "The upstream must be an absolute http or https URI"
  if options.timeout <= TimeSpan.Zero then
    invalidArg (nameof options) "The timeout must be positive"
  let trusted = HashSet<IPAddress>(options.trustedProxies |> Seq.map normalizeIp)
  let forward ctx =
    task {
      use message = buildRequest options trusted upstream ctx
      use timeout = new CancellationTokenSource(options.timeout)
      try
        // `ResponseHeadersRead` keeps the body streaming instead of buffering it.
        // Non-2xx responses come back normally, so there is no error path to unwrap.
        let! response = client.SendAsync(message, HttpCompletionOption.ResponseHeadersRead, timeout.Token)
        return Some (httpResponseToHttpContext ctx response)
      with ex ->
        options.onError ctx ex
        if timeout.IsCancellationRequested then
          return! failWith HTTP_504 "The upstream server did not answer in time." ctx
        else
          return! failWith HTTP_502 "Unable to proxy the request." ctx
    }
  fun ctx ->
    if hasDotSegment ctx.request.rawPath then failWith HTTP_400 "Bad Request" ctx
    else Async.AwaitTask (forward ctx)

/// Forwards requests to `upstream` with `defaultProxyOptions`. See `proxyWith`.
let proxy (upstream : Uri) : WebPart =
  proxyWith defaultProxyOptions upstream