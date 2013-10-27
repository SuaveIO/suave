module Suave.Http

open System
open System.IO
open System.Text
open System.Threading.Tasks

open Utils
open Types

/// Sets a header with the key and value specified
let set_header key value (http_request : HttpRequest) =
  http_request.Response.Headers.Add(key, value)
  http_request

/// Sets a cookie with the passed value in the 'cookie' parameter
let set_cookie cookie = set_header "Set-Cookie" cookie

/// Match on the url
let url    s (x : HttpRequest) = if s = x.Url    then Some x else None
/// Match on the method
let meth0d s (x : HttpRequest) = if s = x.Method then Some x else None

/// Match on GET requests
let GET    (x : HttpRequest) = meth0d "GET" x
/// Match on POST requests
let POST   (x : HttpRequest) = meth0d "POST" x
/// Match on DELETE requests
let DELETE (x : HttpRequest) = meth0d "DELETE" x
/// Match on PUT requests
let PUT    (x : HttpRequest) = meth0d "PUT" x
/// Match on HEAD requests
let HEAD   (x : HttpRequest) = meth0d "HEAD" x
/// Match on CONNECT requests
let CONNECT (x : HttpRequest) = meth0d "CONNECT" x
/// Match on PATCH requests
let PATCH  (x : HttpRequest) = meth0d "PATCH" x
/// Match on TRACE requests
let TRACE  (x : HttpRequest) = meth0d "TRACE" x
/// Match on OPTIONS requests
let OPTIONS (x : HttpRequest) = meth0d "OPTIONS" x

/// The version of the web server
let suave_version = "0.0.3"

/// The protocol version that the server speaks
let proto_version = "HTTP/1.1"

/// Respond with a given status code, http message, content in the body to a http request.
let response_f status_code message (f_content : HttpRequest -> Async<unit>) (request : HttpRequest) = async {
  try
    let stream:Stream = request.Stream

    do! async_writeln stream (sprintf "%s %d %s" proto_version status_code message)
    do! async_writeln stream (sprintf "Server: Suave/%s (http://suaveframework.com)" suave_version)
    do! async_writeln stream (sprintf "Date: %s" (DateTime.UtcNow.ToString("R")))

    for (x,y) in request.Response.Headers do
      if not (List.exists (fun y -> x.ToLower().Equals(y)) ["server";"date";"content-length"]) then
        do! async_writeln stream (sprintf "%s: %s" x y )

    if not(request.Response.Headers.Exists(new Predicate<_>(fun (x,_) -> x.ToLower().Equals("content-type")))) then
      do! async_writeln stream (sprintf "Content-Type: %s" "text/html")

    do! f_content request

  with //the connection might drop while we are sending the response
  | :? IOException as ex  -> raise (InternalFailure "Failure while writing to client stream")
}

/// Respond with a given status code, http message, content in the body to a http request.
let response status_code message (content : byte []) (request : HttpRequest) =
  response_f status_code message (
    fun r -> async {
      if content.Length > 0 then
        do! async_writeln r.Stream (sprintf "Content-Length: %d" content.Length)

      do! async_writeln r.Stream ""

      if content.Length > 0 then
        do! r.Stream.WriteAsync(content, 0, content.Length) })
    request

/// A challenge response with a WWW-Authenticate header,
/// and 401 Authorization Required response message.
let challenge =
  set_header "WWW-Authenticate" "Basic realm=\"protected\""
  >> response 401 "Authorization Required" (bytes "401 Unauthorized.")

/// Write the bytes to the body as a byte array with a 200 OK status-code/message
let ok s = response 200 "OK" s >> succeed

/// Write the string as UTF-8 to the body of the response,
/// with a 200 OK status-code/message.
let OK a = ok (bytes_utf8 a)

/// Write the bytes to the body as a byte array as a 500 Internal Error status-code/message.
let failure message = response 500 "Internal Error" message >> succeed

/// Write the error string as UTF-8 to the body of the response,
/// with a 500 Internal Error status-code/message.
let ERROR a = failure (bytes_utf8 a)

/// Redirect the request to another location specified by the url parameter.
/// Sets the Location header and returns 302 Content Moved status-code/message.
let redirect url =
  set_header "Location" url
  >> response 302 url (bytes "Content Moved")
  >> succeed

/// Send a 404 Not Found with a byte array body specified by the 'message' parameter
let unhandled message = response 404 "Not Found" message >> succeed

/// Write the 'message' string to the body as UTF-8 encoded text, while
/// returning 404 Not Found to the response
let notfound message = unhandled (bytes_utf8 message)

/// Map a file ending to a mime-type
let mime_type = function
  | ".bmp" -> "image/bmp"
  | ".css" -> "text/css"
  | ".gif" -> "image/gif"
  | ".png" -> "image/png"
  | ".ico" -> "image/x-icon"
  | ".htm"
  | ".html" -> "text/html";
  | ".jpe"
  | ".jpeg"
  | ".jpg" -> "image/jpeg"
  | ".js"  -> "application/x-javascript"
  | ".exe" -> "application/exe"
  | _ -> "application/octet-stream"

/// Set the Content-Type header to the mime type given
let set_mime_type t = set_header "Content-Type" t

/// Send a file as a response to the request
let send_file filename r =
  let write_file file (r : HttpRequest) = async {
    use fs = new FileStream(file, FileMode.Open, FileAccess.Read, FileShare.Read)

    if fs.Length > 0L then
      do! async_writeln r.Stream (sprintf "Content-Length: %d" fs.Length)

    do! async_writeln r.Stream ""

    if fs.Length > 0L then
      do! transfer r.Stream fs }

  async { do! response_f 200 "OK" (write_file filename) r } |> succeed

/// Send the file by the filename given. Will search relative to the current directory for
/// the file path, unless you pass it a file with a slash at the start of its name, in which
/// case it will search the root of the file system that is hosting the current directory.
/// Will also set the MIME type based on the file extension.
let file filename =
  if File.Exists filename then
    let file_info = new FileInfo(filename)
    let mimes = mime_type (file_info.Extension)
    set_mime_type mimes >> send_file (filename)
  else
    never

/// Format a string with a local file path given a file name 'fileName'. You should
/// use this helper method to find the current directory and concatenate that current
/// directory to the filename which should be absolute and start with a path separator.
let local_file fileName = sprintf "%s%s" Environment.CurrentDirectory fileName

/// 'browse' the file given as the filename, by sending it to the browser with a
/// MIME-type/Content-Type header based on its extension. Will service from the
/// current directory.
let browse_file filename = file (local_file filename)

/// 'browse' the file in the sense that the contents of the file are sent based on the
/// request's Url property. Will serve from the current directory.
let browse : WebPart = warbler (fun req -> file (local_file req.Url))

type WebResult = Option<Async<unit>>

/// Serve a 'file browser' for a directory
let dir (req : HttpRequest) : WebResult =

  let url = req.Url

  let dirname = local_file url
  let result = new StringBuilder()

  let filesize  (x : FileSystemInfo) =
    if (x.Attributes ||| FileAttributes.Directory = FileAttributes.Directory) then
      String.Format("{0,-14}",System.Web.HttpUtility.HtmlEncode("<DIR>"))
    else
      String.Format("{0,14}", (new FileInfo(x.FullName)).Length)

  let formatdate (t:DateTime) =
    t.ToString("MM-dd-yy") + "  " + t.ToString("hh:mmtt")

  let buildLine (x : FileSystemInfo) =
    result.Append(x.LastWriteTime.ToString() + "       " + filesize(x) + " " + x.Name + "<br/>\n")
    |> ignore

  if Directory.Exists dirname then
    let di = new DirectoryInfo(dirname)
    (di.GetFileSystemInfos()) |> Array.sortBy (fun x -> x.Name) |> Array.iter buildLine
    ok (bytes (result.ToString())) req
  else fail

/// At the location where this function is applied, close the outbound stream
/// for further writing.
let closepipe (p : HttpRequest option) =
  match p with
  | Some(x) ->
    x.Stream.Flush()
    x.Stream.Close()
  | None -> ()

/// Parse the authentication type, the user name and the password from
/// the token string
let parse_authentication_token (token : string) =
  let parts = token.Split (' ')
  let enc = parts.[1].Trim()
  let decoded = decode_base64 enc
  let indexOfColon = decoded.IndexOf(':')
  (parts.[0].ToLower(), decoded.Substring(0,indexOfColon), decoded.Substring(indexOfColon+1))

/// Perform basic authentication on the request, applying a predicate
/// to check the request for authentication tokens such as 'username'
/// and 'password'. Otherwise, if failing, challenge the client again.
let authenticate_basic f (p : HttpRequest) =
  let headers = p.Headers
  if headers.ContainsKey("authorization") then
    let header = headers.["authorization"]
    let (typ,username,password) = parse_authentication_token header
    p.Username <- username
    p.Password <- password
    if (typ.Equals("basic")) && f p then
      fail
    else
      challenge p |> succeed
  else
    challenge p |> succeed

/// Log the HttpRequest to the given stream
let log (s : Stream) (http_request : HttpRequest) =
  let bytes = bytes (sprintf "%A\n" (http_request.Method, http_request.RemoteAddress, http_request.Url, http_request.Query, http_request.Form, http_request.Headers))
  s.Write(bytes, 0, bytes.Length)
  succeed http_request

open Suave.Sscanf

/// Strongly typed route matching! Matching the uri can be used with the 'parsers'
/// characters specified in Sscanf.
let urlscan (pf : PrintfFormat<_,_,_,_,'t>) (h : 't ->  WebPart) : WebPart =
  let t url = sscanf pf url

  let F (r:HttpRequest) =
    try
      let y = r.Url |> t |> h
      try y r with ex -> r |> ERROR (ex.ToString())
    with _ -> fail
  F
