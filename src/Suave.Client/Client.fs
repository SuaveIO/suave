namespace Suave.Client

open System
open System.Diagnostics
open System.IO
open System.Net.Http
open System.Text
open System.Runtime.CompilerServices
open Hopac
open Suave.Core

[<assembly: InternalsVisibleTo "Suave.Client.IntegrationTests">]
[<assembly: InternalsVisibleTo "Suave.Client.UnitTests">]
()

[<AutoOpen>]
module internal Prelude =
  open System

  module Option =
    let orDefault def =
      Option.fold (fun s t -> t) def

  module ASCII =
    open System.Text

    let bytes (s : string) =
      Encoding.ASCII.GetBytes s


module HttpFS =
  open System.IO
  open System.Text
  open System.Security.Cryptography

  [<Measure>] type ms

  let DefaultBodyEncoding = Encoding.UTF8

  // Same as System.Net.DecompressionMethods, but I didn't want to expose that
  type DecompressionScheme =
    | None = 0
    | GZip = 1
    | Deflate = 2

  type ContentRange =
    { start  : int64 option
      finish : int64 option }

  [<CustomComparison; CustomEquality>]
  type ContentType =
    { typ      : string
      subtype  : string
      charset  : Encoding option
      boundary : string option }
  with
    override x.ToString() =
      String.Concat [
        yield x.typ
        yield "/"
        yield x.subtype
        match x.charset with
        | None -> ()
        | Some enc -> yield! [ ";"; " charset="; enc.WebName ]
        match x.boundary with
        | None -> ()
        | Some b -> yield! [ ";"; " boundary="; sprintf "\"%s\"" b ]
      ]

    interface IComparable with
      member x.CompareTo other =
        match other with
        | :? ContentType as ct -> (x :> IComparable<ContentType>).CompareTo ct
        | x -> failwithf "invalid comparison ContentType to %s" (x.GetType().Name)

    interface IComparable<ContentType> with
      member x.CompareTo other =
        match compare x.typ other.typ with
        | 0 -> compare x.subtype x.subtype
        | x -> x

    member x.Equals(typ : string, subtype : string) =
      x.typ = typ && x.subtype = subtype

    override x.Equals o =
      match o with
      | :? ContentType as ct -> (x :> IEquatable<ContentType>).Equals ct
      | _ -> false

    interface IEquatable<ContentType> with
      member x.Equals ct =
        x.typ = ct.typ
        && x.subtype = ct.subtype
        && (x.charset |> Option.orDefault DefaultBodyEncoding) = (ct.charset |> Option.orDefault DefaultBodyEncoding)

    override x.GetHashCode () =
      397 * hash x.typ
      ^^^ hash x.subtype

    static member create(typ : string, subtype : string, ?charset : Encoding, ?boundary : string) =
      { typ      = typ
        subtype  = subtype
        charset  = charset
        boundary = boundary }

    // TODO, use: https://github.com/freya-fs/freya/blob/master/src/Freya.Types.Http/Types.fs#L420-L426
    static member parse (str : string) =
      match str.Split [| '/' |], str.IndexOf(';') with
      | [| typ; subtype |], -1 ->
        Some { typ = typ; subtype = subtype; charset = None; boundary = None }
      | [| typ; rest |], index ->
        Some { typ = typ; subtype = rest.Substring(0, index); charset = None; boundary = None }
      | x -> None

  type ResponseHeader =
    | AccessControlAllowOrigin
    | AcceptRanges
    | Age
    | Allow
    | CacheControl
    | Connection
    | ContentEncoding
    | ContentLanguage
    | ContentLength
    | ContentLocation
    | ContentMD5Response
    | ContentDisposition
    | ContentRange
    | ContentTypeResponse
    | DateResponse
    | ETag
    | Expires
    | LastModified
    | Link
    | Location
    | P3P
    | PragmaResponse
    | ProxyAuthenticate
    | Refresh
    | RetryAfter
    | Server
    | SetCookie
    | StrictTransportSecurity
    | Trailer
    | TransferEncoding
    | Vary
    | ViaResponse
    | WarningResponse
    | WWWAuthenticate
    | NonStandard of string
  with
    override x.ToString() =
      match x with
      | AccessControlAllowOrigin -> "Access-Control-Allow-Origin"
      | AcceptRanges -> "Accept-Ranges"
      | Age -> "Age"
      | Allow -> "Allow"
      | CacheControl -> "Cache-Control"
      | Connection -> "Connection"
      | ContentEncoding -> "Content-Encoding"
      | ContentLanguage -> "Content-Language"
      | ContentLength -> "Content-Length"
      | ContentLocation -> "Content-Location"
      | ContentMD5Response -> "Content-MD5-Response"
      | ContentDisposition -> "Content-Disposition"
      | ContentRange -> "Content-Range"
      | ContentTypeResponse -> "Content-Type-Response"
      | DateResponse -> "Date-Response"
      | ETag -> "ETag"
      | Expires -> "Expires"
      | LastModified -> "Last-Modified"
      | Link -> "Link"
      | Location -> "Location"
      | P3P -> "P3P"
      | PragmaResponse -> "Pragma-Response"
      | ProxyAuthenticate -> "Proxy-Authenticate"
      | Refresh -> "Refresh"
      | RetryAfter -> "Retry-After"
      | Server -> "Server"
      | SetCookie -> "Set-Cookie"
      | StrictTransportSecurity -> "Strict-Transport-Security"
      | Trailer -> "Trailer"
      | TransferEncoding -> "Transfer-Encoding"
      | Vary -> "Vary"
      | ViaResponse -> "Via-Response"
      | WarningResponse -> "Warning-Response"
      | WWWAuthenticate -> "WWW-Authenticate"
      | NonStandard str -> str

  // some headers can't be set with HttpClient, or are set automatically, so are not included.
  // others, such as transfer-encoding, just haven't been implemented.
  type RequestHeader =
    | Accept of string
    | AcceptCharset of string
    | AcceptDatetime of string
    | AcceptLanguage of string
    | Authorization of string
    | Connection of string
    | ContentMD5 of string
    | ContentType of ContentType
    | Date of DateTime
    | Expect of int
    | From of string
    | IfMatch of string
    | IfModifiedSince of DateTime
    | IfNoneMatch of string
    | IfRange of string
    | MaxForwards of int
    | Origin of string
    | Pragma of string
    | ProxyAuthorization of string
    | Range of ContentRange
    | Referer of string
    | Upgrade of string
    | UserAgent of string
    | Via of string
    | Warning of string
    | Custom of string * string

    member x.KeyValue : string * string =
      match x with
      | Accept x -> "Accept", x
      | AcceptCharset x -> "Accept-Charset", x
      | AcceptDatetime x -> "Accept-Datetime", x
      | AcceptLanguage x -> "Accept-Language", x
      | Authorization x -> "Authorization", x
      | Connection x -> "Connection", x
      | ContentMD5 x -> "Content-MD5", x
      | ContentType x -> "Content-Type", x.ToString()
      | Date dt -> "Date", dt.ToString("R")
      | Expect i -> "Expect", i.ToString()
      | From x -> "From", x
      | IfMatch x -> "If-Match", x
      | IfModifiedSince dt -> "If-Modified-Since", dt.ToString("R")
      | IfNoneMatch x -> "If-None-Match", x
      | IfRange x -> "If-Range", x
      | MaxForwards i -> "Max-Forwards", string i
      | Origin x -> "Origin", x
      | Pragma x -> "Pragma", x
      | ProxyAuthorization x -> "Proxy-Authorization", x
      | Range { start = Some s; finish = None } -> "Range", "bytes=" + string s
      | Range { start = Some s; finish = Some f } -> "Range", sprintf "bytes=%d-%d" s f
      | Range { start = _; finish = _ } -> "Range", ""
      | Referer x -> "Referer", x
      | Upgrade x -> "Upgrade", x
      | UserAgent x -> "User-Agent", x
      | Via x -> "Via", x
      | Warning x -> "Warning", x
      | Custom (n, v) -> n, v

    member x.Key = x.KeyValue |> fst

  type UserDetails =
    { username : string
      password : string }

  [<RequireQualifiedAccess>]
  type Credentials =
    | None
    | Default
    | Custom of UserDetails

  type Proxy =
    { Address: string
      Port: int
      Credentials: Credentials }

  /// The key you have in &lt;input name="key" ... /&gt;
  /// This string value is not yet encoded.
  type FormEntryName = string

  /// The string value is not yet encoded.
  type FormValue = string

  /// The key of a query string key-value pair.
  /// The string value is not yet encoded.
  type QueryStringName = string

  /// The string value is not yet encoded.
  type QueryStringValue = string

  /// http://www.w3.org/Protocols/rfc2616/rfc2616-sec19.html
  /// section 19.5.1 Content-Disposition, BNF.
  type ContentDisposition =
    { typ      : string // "form-data" or "attachment"
      filename : string option
      /// e.g. "name=user_name"
      exts     : (FormEntryName * FormValue) list }

  /// An optional file name
  type FileName = string

  type FileData =
    | Plain of string
    | Binary of byte []
    | StreamData of Stream

  /// A file is a file name, a content-type (application/octet-stream if unknown) and the data.
  type File = FileName * ContentType * FileData

  /// http://www.w3.org/TR/html401/interact/forms.html
  type FormData =
    /// Use when you post a single file
    /// Will use: multipart/form-data
    | FormFile of name:FormEntryName * File
    /// Use when you post multiple files as a multi-file-browse control
    /// Will use: multipart/mixed inside a multipart/form-data.
    | MultipartMixed of name:FormEntryName * files:File list
    /// Use when you simply post form data
    | NameValue of FormEntryName * FormValue

  /// You often pass form-data to the server, e.g. curl -X POST <url> -F k=v -F file1=@file.png
  type Form = FormData list

  type RequestBody =
    | BodyForm of Form // * TransferEncodingHint option (7bit/8bit/binary)
    | BodyString of string
    | BodyRaw of byte []
      //| BodySocket of SocketTask // for all the nitty-gritty details, see #64

  /// The name (key) of a cookie.
  /// The string value is unencoded.
  type CookieName = string

  type Cookie =
    { name     : CookieName
      value    : string
      expires  : DateTimeOffset option
      path     : string option
      domain   : string option
      secure   : bool
      httpOnly : bool }

    static member create(name : CookieName, value : string, ?expires, ?path, ?domain, ?secure, ?httpOnly) =
      { name     = name
        value    = value
        expires  = expires
        path     = path
        domain   = domain
        secure   = defaultArg secure false
        httpOnly = defaultArg httpOnly false }

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Cookie =
    let internal toSystem x =
      let sc = System.Net.Cookie(x.name, x.value, Option.orDefault "/" x.path, Option.orDefault "" x.domain)
      x.expires |> Option.iter (fun e -> sc.Expires <- e.DateTime)
      sc.HttpOnly <- x.httpOnly
      sc.Secure <- x.secure
      sc

  type Request =
    { url                       : Uri
      ``method``                : HttpMethod
      cookiesEnabled            : bool
      headers                   : Map<string, RequestHeader>
      body                      : RequestBody
      bodyCharacterEncoding     : Encoding
      queryStringItems          : Map<QueryStringName, QueryStringValue list>
      cookies                   : Map<CookieName, Cookie>
      responseCharacterEncoding : Encoding option
      proxy                     : Proxy option
      httpClient                : HttpClient }

  type CharacterSet = string

  type Response =
    { statusCode       : int
      contentLength    : int64 option
      characterSet     : CharacterSet option
      cookies          : Map<string, string>
      headers          : Map<ResponseHeader, string>
      /// A Uri that contains the URI of the Internet resource that responded to the request.
      /// <see cref="https://msdn.microsoft.com/en-us/library/system.net.httpwebresponse.responseuri%28v=vs.110%29.aspx"/>.
      expectedEncoding : Encoding option
      responseUri      : System.Uri
      body             : Stream
      luggage          : IDisposable option }

    interface IDisposable with
      member x.Dispose() =
        x.body.Dispose()
        x.luggage |> Option.iter (fun x -> x.Dispose())

    override x.ToString() =
      seq {
        yield x.statusCode.ToString()
        for h in x.headers do
          yield h.ToString()
        yield ""
        //if x.EntityBody |> Option.isSome then
        //     yield x.EntityBody |> Option.get
      } |> String.concat Environment.NewLine

  let defaultLogger =
    Suave.Logging.Log.create "HttpFs"

  type HttpFsState =
    { random      : Random
      cryptRandom : RandomNumberGenerator
      logger      : HttpFs.Logging.Logger }

    /// Will re-generate random CLR per-app-domain -- create your own state for
    /// deterministic boundary generation (or anything else needing random).
    static member empty =
      { random      = Random()
        cryptRandom = RandomNumberGenerator.Create()
        logger      = defaultLogger }

  /// The header you tried to add was already there, see issue #64.
  exception DuplicateHeader of RequestHeader

  module internal Impl =
    open System.Collections.Generic

    [<Literal>]
    let CRLF = "\r\n"

    let ISOLatin1 = Encoding.GetEncoding "ISO-8859-1"

    let getMethodAsString request =
      match request.``method`` with
      | Options -> "OPTIONS"
      | Get -> "GET"
      | Head -> "HEAD"
      | Post -> "POST"
      | Put -> "PUT"
      | Delete -> "DELETE"
      | Trace -> "TRACE"
      | Connect -> "CONNECT"
      | Patch   -> "PATCH"

    /// URI encoding: for each byte in the byte-representation of the string,
    /// print the %xx character as an ASCII character, for transfer.
    let uriEncode : _ -> string =
      List.map (fun kv ->
        String.Concat [
          WebUtility.UrlEncode (fst kv)
          "="
          WebUtility.UrlEncode (snd kv)
        ])
      >> String.concat "&"

    let getQueryString request =
      if Map.isEmpty request.queryStringItems then ""
      else
        let items =
          Map.toList request.queryStringItems
          |> List.collect (fun (k, vs) -> vs |> List.map (fun v -> k,v))
        String.Concat [ uriEncode items ]

    let basicAuthorz username password =
      String.Concat [ username; ":"; password ]
      |> DefaultBodyEncoding.GetBytes
      |> Convert.ToBase64String
      |> fun base64 -> "Basic " + base64
      |> Authorization

    let generateBoundary =
      let boundaryChars = "abcdefghijklmnopqrstuvwxyz+-/':ABCDEFGHIJKLMNOPQRSTUVWXYZ"
      let boundaryLen = 30
      fun clientState ->
          let rnd = clientState.random
          let sb = StringBuilder(boundaryLen)
          for i in 0 .. boundaryLen - 1 do
              sb.Append (boundaryChars.[rnd.Next(boundaryChars.Length)]) |> ignore
          sb.ToString()

    let escapeQuotes (s : string) =
        // https://github.com/rack/rack/issues/323#issuecomment-3609743
        s.Replace("\"", "\\\"")

    module StreamWriters =
      let writeBytes bs (output : Stream) =
        Job.awaitUnitTask (output.WriteAsync(bs, 0, bs.Length))

      let writeBytesLine bs (output : Stream) =
        job {
          do! writeBytes bs output
          do! Job.awaitUnitTask (output.WriteAsync (ASCII.bytes CRLF, 0, 2))
        }

      /// Writes a string and CRLF as ASCII
      let writeLineAscii string : Stream -> Job<unit> =
        String.Concat [ string; CRLF ] |> ASCII.bytes |> writeBytes

      /// Writes a string as ASCII
      let writeAscii : string -> Stream -> Job<unit> =
        ASCII.bytes >> writeBytes

      /// Writes a string and CRLF as UTF8
      let writeLineUtf8 string =
        String.Concat [ string; CRLF ] |> UTF8.bytes |> writeBytes

      /// Writes a string as UTF8
      let writeUtf8 : string -> Stream -> Job<unit> =
        UTF8.bytes >> writeBytes

      let writeStream (input : Stream) (output : Stream) =
        Job.awaitUnitTask (input.CopyToAsync output)

      let writeStreamLine input output =
        job {
          do! writeStream input output
          do! Job.awaitUnitTask (output.WriteAsync (ASCII.bytes CRLF, 0, 2))
        }

    open StreamWriters

    let generateFileData (encoding : Encoding) contentType contents = seq {
      match contentType, contents with
      | { typ = "text"; subtype = _ }, Plain text ->
        yield writeLineAscii ""
        yield writeLineUtf8 text

      | { typ = "application"; subtype = subtype }, Plain text
        when List.exists ((=) (subtype.Split('+') |> Seq.last)) ["json"; "xml"] ->
        yield writeLineAscii ""
        yield writeLineUtf8 text

      | _, Plain text ->
        yield writeLineAscii "Content-Transfer-Encoding: base64"
        yield writeLineAscii ""
        yield writeLineAscii (text |> encoding.GetBytes |> Convert.ToBase64String)

      | _, Binary bytes ->
        yield writeLineAscii "Content-Transfer-Encoding: binary"
        yield writeLineAscii ""
        yield writeBytesLine bytes

      | _, StreamData stream ->
        yield writeLineAscii "Content-Transfer-Encoding: binary"
        yield writeLineAscii ""
        yield writeStreamLine stream
    }

    let generateContentDispos value (kvs : (string * string) list) =
        let formatKv = function
            | k, v -> (sprintf "%s=\"%s\"" k (escapeQuotes v))
        String.concat "; " [ yield sprintf "Content-Disposition: %s" value
                             yield! (kvs |> List.map formatKv) ]

    let generateFormData state (encoding : Encoding) boundary formData =
      let rec generateFormDataInner boundary values isMultiFile = [
        match values with
        | [] ->
          yield writeLineAscii (sprintf "--%s--" boundary)
        | h :: rest ->
          yield writeLineAscii (sprintf "--%s" boundary)
          match h with
          | FormFile (name, (fileName, contentType, contents)) ->
            let dispos = if isMultiFile then "file" else "form-data"
            yield writeLineUtf8 (generateContentDispos dispos
                                  [ if not isMultiFile then yield "name", name
                                    yield "filename", fileName ])
            yield writeLineUtf8 (sprintf "Content-Type: %O" contentType)
            yield! generateFileData encoding contentType contents

          | MultipartMixed (name, files) ->
            let boundary' = generateBoundary state
            yield writeLineAscii (sprintf "Content-Type: multipart/mixed; boundary=\"%s\"" boundary')
            yield writeLineUtf8 (generateContentDispos "form-data" [ "name", name ])
            yield writeLineUtf8 ""
            // remap the multi-files to single files and recursively call myself
            let files' = files |> List.map (fun f -> FormFile (name, f))
            yield! generateFormDataInner boundary' files' true

          | NameValue (name, value) ->
            yield writeLineAscii (sprintf "Content-Disposition: form-data; name=\"%s\"" (escapeQuotes name))
            yield writeLineAscii ""
            yield writeLineUtf8 value
          yield! generateFormDataInner boundary rest isMultiFile
      ]
      generateFormDataInner boundary formData false

    let private formatBodyUrlencoded formData =
      [ formData
        |> List.map (function
            | NameValue (k, v) -> k, v
            | x -> failwith "programming error: expected all formData to be NameValue as per 'formatBody'.")
        |> uriEncode
        // after URI encoding, we represent all bytes in ASCII (subset of Latin1)
        // and none-the-less; they will map 1-1 with the UTF8 set if the server
        // interpret Content-Type: ...; charset=utf8 as 'raw bytes' of the body.
        |> ISOLatin1.GetBytes
        |> writeBytes
      ]

    let formatBody (clientState : HttpFsState) =
      // we may actually change the content type if it's wrong
      //: ContentType option * Encoding * RequestBody -> ContentType option * byte [] =
      function
      | userCt, _, BodyRaw raw ->
        userCt, [ writeBytes raw ]

      | userCt, _, BodyString str ->
        userCt, [ writeUtf8 str ]

      | userCt, _, BodyForm [] ->
        userCt, [ writeBytes [||] ]

      | userCt, encoding, BodyForm formData ->
        let onlyNameValues =
          formData |> List.forall (function | NameValue _ -> true | _ -> false)

        if onlyNameValues then
          ContentType.parse "application/x-www-form-urlencoded",
          formatBodyUrlencoded formData
        else
          let boundary = generateBoundary clientState
          ContentType.create("multipart", "form-data", boundary=boundary) |> Some,
          generateFormData clientState encoding boundary formData

  open Impl

  module internal DotNetWrapper =
    open System.Collections.Generic
    let defaultHttpClient =
      // the handler cannot be changed after the first request (you get an exception).
      // kept internal

      // UseCookies = false stops the handler from using it's own CookieContainer.
      // this stops it from stripping "Cookie" headers we set out of requests and will stop
      // cookies being implicitly shared across requests
      let handler = new HttpClientHandler(UseCookies = false)
      let client = new HttpClient(handler)
      client.DefaultRequestHeaders.Clear()
      client

    /// Sets headers on HttpRequestMessage.
    /// Mutates HttpRequestMessage.
    let setHeaders (headers : RequestHeader list) (request : HttpRequestMessage) =
      let add (k : string) (v: string) = request.Headers.Add (k, v)
      // in .NET full, HttpRequestMessage cannot have the Content property with HttpMethods that do not support content, or it fails with an exception,
      // so request.Content can be null. this is acutally NOT the case in .NET Core, where it does not throw an exception when content is set with a GET method
      let addContent (add: HttpContent -> unit) = if not <| isNull request.Content then add request.Content
      List.iter (function
                | Accept value                     -> add "Accept" value
                | AcceptCharset value              -> add "Accept-Charset" value
                | AcceptDatetime value             -> add "Accept-Datetime" value
                | AcceptLanguage value             -> add "Accept-Language" value
                | Authorization value              -> add "Authorization" value
                | RequestHeader.Connection value   -> add "Connection" value
                | RequestHeader.ContentMD5 value   -> addContent (fun c ->
                                                        // this is to work around a mono bug.
                                                        // when setting "content-md5" manually, mono writes "Sytem.Byte[]" as the value.
                                                        // setting the ContentMD5 property works the same across frameworks, however it will
                                                        // convert the provided value to a base64 string automagically
                                                        let bytes = Convert.FromBase64String(value)
                                                        c.Headers.ContentMD5 <- bytes)
                | RequestHeader.ContentType value  -> addContent (fun c ->
                                                        c.Headers.Remove("Content-Type") |> ignore
                                                        c.Headers.TryAddWithoutValidation("Content-Type", value.ToString()) |> ignore)
                | RequestHeader.Date value         -> add "Date" <| value.ToUniversalTime().ToString("r")
                | Expect value                     -> add "Expect" <| value.ToString()
                | From value                       -> add "From" value
                | IfMatch value                    -> add "If-Match" value
                | IfModifiedSince value            -> add "If-Modified-Since" <| value.ToUniversalTime().ToString("r")
                | IfNoneMatch value                -> add "If-None-Match" value
                | IfRange value                    -> add "If-Range" value
                | MaxForwards value                -> add "Max-Forwards" <| value.ToString()
                | Origin value                     -> add "Origin" value
                | RequestHeader.Pragma value       -> add "Pragma" value
                | ProxyAuthorization value         -> add "Proxy-Authorization" value
                | Range value                      -> let range = new Headers.RangeHeaderValue(Option.toNullable value.start, Option.toNullable value.finish)
                                                      request.Headers.Range <- range
                | Referer value                    -> add "Referer" value
                | Upgrade value                    -> add "Upgrade" value
                | UserAgent value                  -> add "User-Agent" value
                | RequestHeader.Via value          -> add "Via" value
                | RequestHeader.Warning value      -> add "Warning" value
                | Custom (customName, customValue) -> add customName customValue)
                headers

    // Sets cookies on HttpRequestMessage.
    // Mutates HttpRequestMessage.
    let setCookies (cookies : Cookie list) (url : Uri) (requestMessage: HttpRequestMessage) =
      let mapDomain c = { c with domain = Some url.Host }
      let cookieStr =
        cookies
        |> List.map ((mapDomain >> Cookie.toSystem) >> (fun c -> sprintf "%s=%s" c.Name c.Value))
        |> String.concat "; "

      if (cookieStr.Length > 0) then
        requestMessage.Headers.Add("Cookie", cookieStr)

    /// Sets body on HttpWebRequest.
    /// Mutates HttpWebRequest.
    let tryWriteBody (method: HttpMethod) (writers : seq<Stream -> Job<unit>>) (contentStream : Stream) =
      job {
        for writer in writers do
          do! writer contentStream
      }

    let matchCtHeader k = function
      | RequestHeader.ContentType ct -> Some ct
      | _ -> None

    let ensureNo100Continue () =
      if ServicePointManager.Expect100Continue then
        ServicePointManager.Expect100Continue <- false

    let setHeader (request : Request) (header : RequestHeader) =
      { request with headers = request.headers |> Map.add header.Key header }

    /// The nasty business of turning a Request into an HttpRequestMessage
    let toHttpRequestMessage state (contentStream: Stream) (request : Request) =
      ensureNo100Continue ()

      let contentType = request.headers |> Map.tryPick matchCtHeader

      let contentEncoding =
        // default the ContentType charset encoding, otherwise, use BodyCharacterEncoding.
        contentType
        |> function
        | Some { charset = Some enc } -> Some enc
        | _ -> None
        |> Option.fold (fun s t -> t) request.bodyCharacterEncoding

      let url =
        let b = UriBuilder request.url
        match b.Query with
        | "" | null -> b.Query <- getQueryString request
        | _ -> ()
        b.Uri

      let methodStr = getMethodAsString request
      let method = new Net.Http.HttpMethod(methodStr)
      let message = new HttpRequestMessage(method, url, Version = HttpVersion.Version11)

      let newContentType, writers =
        formatBody state (contentType, contentEncoding, request.body)

      let request =
        // if we have a new content type, from using createContent, then this
        // updates the request value with that header
        newContentType
        |> Option.map RequestHeader.ContentType
        |> Option.fold setHeader request

      job {
        if request.method = Post || request.method = Put || request.method = Patch then
          do! contentStream |> tryWriteBody request.method writers
          contentStream.Position <- 0L
          message.Content <- new StreamContent(contentStream)

        if (request.cookiesEnabled) then
          message |> setCookies (request.cookies |> Map.toList |> List.map snd) request.url

        message |> setHeaders (request.headers |> Map.toList |> List.map snd)
        return message
      }

    /// For debugging purposes only
    /// Converts the Request body to a format suitable for HttpRequestMessage and returns this raw body as a string.
    let getRawRequestBodyString (state : HttpFsState) (request : Request) =
      let contentType = request.headers |> Map.tryPick matchCtHeader
      let contentEncoding =
        // default the ContentType charset encoding, otherwise, use BodyCharacterEncoding.
        contentType
        |> function
        | Some { charset = Some enc } -> Some enc
        | _ -> None
        |> Option.fold (fun s t -> t) request.bodyCharacterEncoding

      let newContentType, body =
        formatBody state (contentType, contentEncoding, request.body)

      use dataStream = new IO.MemoryStream()
      job {
          for writer in body do
            do! writer dataStream
        } |> Hopac.run

      dataStream.Position <- 0L // Reset stream position before reading
      use reader = new IO.StreamReader(dataStream)
      reader.ReadToEnd()

    /// Uses the HttpClient to send the HttpRequestMessage and get the response.

    let getResponseNoException (httpClient: HttpClient) (request : HttpRequestMessage) : Alt<Choice<HttpResponseMessage,exn>> =
      let get = Alt.fromTask (fun cts -> httpClient.SendAsync(request, cts))
      Alt.tryIn get (Choice1Of2 >> Job.result) (Choice2Of2 >> Job.result)

    let getCookiesAsMap (response: HttpResponseMessage) =
      let uri = response.RequestMessage.RequestUri
      let container = new System.Net.CookieContainer()

      let tryCookies = ref (new List<string>() :> IEnumerable<string>)
      if response.Headers.TryGetValues("Set-Cookie", tryCookies) then
        tryCookies.Value
        |> Seq.iter(fun c -> container.SetCookies(response.RequestMessage.RequestUri, c))

        let cookies = container.GetCookies(uri)
        let cookieArray = Array.zeroCreate cookies.Count
        cookies.CopyTo(cookieArray, 0)

        cookieArray |> Array.fold (fun map cookie -> map |> Map.add cookie.Name cookie.Value) Map.empty
      else
        Map.empty

    /// Get the header as a ResponseHeader option. Is an option because there are some headers we don't want to set.
    let getResponseHeader = function
      | null -> None
      | "Access-Control-Allow-Origin" -> Some(AccessControlAllowOrigin)
      | "Accept-Ranges"               -> Some(AcceptRanges)
      | "Age"                         -> Some(Age)
      | "Allow"                       -> Some(Allow)
      | "Cache-Control"               -> Some(CacheControl)
      | "Connection"                  -> Some(ResponseHeader.Connection)
      | "Content-Encoding"            -> Some(ContentEncoding)
      | "Content-Language"            -> Some(ContentLanguage)
      | "Content-Length"              -> Some(ContentLength)
      | "Content-Location"            -> Some(ContentLocation)
      | "Content-MD5"                 -> Some(ResponseHeader.ContentMD5Response)
      | "Content-Disposition"         -> Some(ContentDisposition)
      | "Content-Range"               -> Some(ContentRange)
      | "Content-Type"                -> Some(ResponseHeader.ContentTypeResponse)
      | "Date"                        -> Some(ResponseHeader.DateResponse)
      | "ETag"                        -> Some(ETag)
      | "Expires"                     -> Some(Expires)
      | "Last-Modified"               -> Some(LastModified)
      | "Link"                        -> Some(Link)
      | "Location"                    -> Some(Location)
      | "P3P"                         -> Some(P3P)
      | "Pragma"                      -> Some(ResponseHeader.PragmaResponse)
      | "Proxy-Authenticate"          -> Some(ProxyAuthenticate)
      | "Refresh"                     -> Some(Refresh)
      | "Retry-After"                 -> Some(RetryAfter)
      | "Server"                      -> Some(Server)
      | "Set-Cookie"                  -> Some(SetCookie)
      | "Strict-Transport-Security"   -> Some(StrictTransportSecurity)
      | "Trailer"                     -> Some(Trailer)
      | "Transfer-Encoding"           -> Some(TransferEncoding)
      | "Vary"                        -> Some(Vary)
      | "Via"                         -> Some(ResponseHeader.ViaResponse)
      | "Warning"                     -> Some(ResponseHeader.WarningResponse)
      | "WWW-Authenticate"            -> Some(WWWAuthenticate)
      | name                          -> Some(NonStandard name)

    /// Gets the headers from the passed response as a map of ResponseHeader and string.
    let getHeadersAsMap (response: HttpResponseMessage) =
      (response.Content.Headers |> List.ofSeq) @ (response.Headers |> List.ofSeq)
      |> List.choose (fun header ->
        getResponseHeader header.Key
        |> Option.map (fun h -> (h, header.Value |> String.concat ", ")))
      |> Map.ofList

    let mapEncoding = String.toLowerInvariant >> function
      | "utf8" -> "utf-8"
      | "utf16" -> "utf-16"
      | other -> other

    let getCharacterSet (response: HttpResponseMessage) =
      let ct = response.Content.Headers.ContentType
      match ct with
      | null -> None
      | _ when String.IsNullOrEmpty ct.CharSet -> None
      | _ -> Some ct.CharSet

  open DotNetWrapper

  type Response with
    static member internal ofHttpResponseMessage (response: HttpResponseMessage) =
      job {
        let! bodyStream = Job.fromTask response.Content.ReadAsStreamAsync
        return
          { statusCode       = int (response.StatusCode)
            characterSet     = getCharacterSet response
            contentLength    = Option.ofNullable response.Content.Headers.ContentLength
            cookies          = getCookiesAsMap response
            headers          = getHeadersAsMap response
            responseUri      = response.RequestMessage.RequestUri
            expectedEncoding = None
            body             = bodyStream
            luggage          = Some (upcast response) }
      }

  let tryGetResponse request =
    let prepare = job {
      use ms = new MemoryStream()
      use! requestMessage = request |> toHttpRequestMessage HttpFsState.empty ms
      let! response = requestMessage |> getResponseNoException request.httpClient

      match response with
      | Choice1Of2 x ->
        let! resp = Response.ofHttpResponseMessage x
        return Alt.once <| Choice1Of2 { resp with expectedEncoding = request.responseCharacterEncoding }
      | Choice2Of2 x -> return Alt.once <| Choice2Of2 x
    }

    Alt.prepare prepare

  /// Sends the HTTP request and returns the full response as a Response record, asynchronously.
  let getResponse request =
    let getResponseOrFail = function
      | Choice1Of2 resp -> resp
      | Choice2Of2 exn -> raise <| new Exception("Failed to get response", exn)
    tryGetResponse request
    |> Alt.afterFun getResponseOrFail

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Response =
    let readBodyAsString (response : Response) : Job<string> =
      job {
        let charset =
          match response.expectedEncoding with
          | None ->
            match response.characterSet with
            | None ->
              ISOLatin1 // TODO: change to UTF-8
            | Some responseCharset ->
              try Encoding.GetEncoding(mapEncoding responseCharset)
              with _ -> Encoding.UTF8

          | Some enc ->
            enc

        use sr = new StreamReader(response.body, charset)
        return! sr.ReadToEndAsync()
      }

    let readBodyAsBytes (response : Response) : Job<byte []> =
      job {
        use ms = new MemoryStream()
        do! Job.awaitUnitTask (response.body.CopyToAsync ms)
        return ms.ToArray()
      }

  /// For those of you who can't be bothered to use getResponse |> Response.readBodyAsString.
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module Request =

    /// <summary>Creates the Request record which can be used to make an HTTP request</summary>
    /// <param name="httpMethod">The type of request to be made (Get, Post, etc.)</param>
    /// <param name="url">The URL of the resource including protocol, e.g. 'http://www.relentlessdevelopment.net'</param>
    /// <returns>The Request record</returns>
    let create httpMethod (url : Uri) =
      { url                       = url
        ``method``                = httpMethod
        cookiesEnabled            = true
        headers                   = Map.empty
        body                      = BodyRaw [||]
        bodyCharacterEncoding     = DefaultBodyEncoding
        queryStringItems          = Map.empty
        cookies                   = Map.empty
        responseCharacterEncoding = None
        proxy                     = None
        httpClient                = defaultHttpClient }

    let createWithClient client method url =
      { create method url with httpClient = client }

    let createUrl httpMethod url =
      create httpMethod (Uri url)

    // Adds `resource` to `request` url
    let path resource (request: Request) =
      let ub = UriBuilder request.url
      ub.Path <- resource
      { request with url = ub.Uri}

    /// Adds 'resourceToAdd' to an url of 'request'
    let resource resourceToAssign request =
      path resourceToAssign request

    /// Sets 'methodToAssign' to a method of 'request'
    let setMethod methodToAssign request =
      { request with ``method`` = methodToAssign}

    /// Disables cookies, which are enabled by default
    let cookiesDisabled request =
      { request with cookiesEnabled = false }

    /// Adds a header, defined as a RequestHeader
    /// The current implementation doesn't allow you to add a single header multiple
    /// times. File an issue if this is a limitation for you.
    let setHeader (header : RequestHeader) (request : Request) =
      { request with headers = request.headers |> Map.add header.Key header }

    /// Adds an HTTP Basic Authentication header, which includes the username and password encoded as a base-64 string
    let basicAuthentication username password =
      setHeader (basicAuthorz username password)

    /// Lets you set your own body - use the RequestBody type to build it up.
    let body body (request : Request) =
      { request with body = body }

    /// Sets the the request body, using UTF-8 character encoding.
    ///
    /// Only certain request types should have a body, e.g. Posts.
    let bodyString body (request : Request) =
      { request with body = BodyString body }

    /// Sets the request body, using the provided character encoding.
    let bodyStringEncoded body characterEncoding request =
      { request with body = BodyString body; bodyCharacterEncoding = characterEncoding }

    /// Adds the provided QueryString record onto the request URL.
    /// Multiple items can be appended.
    let queryStringItem (name : QueryStringName) (value : QueryStringValue) request =
      { request with queryStringItems =
                        match request.queryStringItems |> Map.tryFind name with
                        | None -> request.queryStringItems |> Map.add name [value]
                        | Some vs -> request.queryStringItems |> Map.add name (value::vs) }

    /// Adds a cookie to the request
    /// The domain will be taken from the URL, and the path set to '/'.
    ///
    /// If your cookie appears not to be getting set, it could be because the response is a redirect,
    /// which (by default) will be followed automatically, but cookies will not be re-sent.
    let cookie cookie request =
      if not request.cookiesEnabled then failwithf "Cannot add cookie %A - cookies disabled" cookie.name
      { request with cookies = request.cookies |> Map.add cookie.name cookie }

    /// Decodes the response using the specified encoding, regardless of what the response specifies.
    ///
    /// If this is not set, response character encoding will be:
    ///  - taken from the response content-encoding header, if provided, otherwise
    ///  UTF8
    ///
    /// Many web pages define the character encoding in the HTML. This will not be used.
    let responseCharacterEncoding encoding request : Request =
      { request with responseCharacterEncoding = Some encoding }

    /// Sends the request via the provided proxy.
    ///
    /// If this is no set, the proxy settings from IE will be used, if available.
    let proxy proxy request =
      {request with proxy = Some proxy }

    /// Note: this sends the request, reads the response, disposes it and its stream
    let responseAsString req = job {
      use! resp = getResponse req
      return! Response.readBodyAsString resp
    }

    /// Note: this sends the request, reads the response, disposes it and its stream
    let responseAsBytes req = job {
      use! resp = getResponse req
      return! Response.readBodyAsBytes resp
    }

/// Module that encapsulates common compositional patterns
module Composition =

  open Hopac.Infixes
  open HttpFs.Logging
  open Client
  open System.Diagnostics

  type JobFunc<'a, 'b> = 'a -> Alt<'b>

  module JobFunc =
    let map (f : 'b -> 'c) (func : JobFunc<'a, 'b>) : JobFunc<'a, 'c> =
      func >> Alt.afterFun f

    let mapLeft (f : 'a -> 'c) (func : JobFunc<'c, 'b>) : JobFunc<'a, 'b> =
      f >> func

  type JobFilter<'a, 'b, 'c, 'd> = JobFunc<'a, 'b> -> JobFunc<'c, 'd>
  type JobFilter<'a, 'b> = JobFilter<'a, 'b, 'a, 'b>
  type JobSink<'a> = JobFunc<'a, unit>

  let identify clientName : JobFilter<Request, Response> =
    fun service ->
      Request.setHeader (RequestHeader.UserAgent clientName)
      >> service

  let around before after : JobFilter<_, _> =
    fun func req ->
      Alt.prepareFun (fun () ->
        let pX = before ()
        func req ^-> fun res ->
        after pX
        res)

  let timerFilter (state : HttpFsState) : JobFilter<Request, Response> =
    around (fun () -> Stopwatch.StartNew())
           (fun sw -> sw.Stop(); Message.gauge sw.ElapsedTicks "ticks" |> state.logger.logSimple)

  let timerFilterNamed (state : HttpFsState) (name) : JobFilter<Request, Response> =
    around (fun () -> Stopwatch.StartNew())
           (fun sw ->
             sw.Stop()
             Message.gauge sw.ElapsedTicks "ticks"
             |> Message.setName name
             |> state.logger.logSimple)

  let codecFilter (enc, dec) : JobFilter<'i, 'o, Request, Response> =
    JobFunc.mapLeft enc
    >> JobFunc.map dec