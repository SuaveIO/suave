
open System
open System.Net

open Suave
open Suave.Web
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Files
open Suave.Http.Successful
open Suave.Types
open Suave.Session
open Suave.Log

open OpenSSL.X509
open OpenSSL.Core

open Suave.OpenSsl.Provider

let basic_auth : WebPart =
  Authentication.authenticate_basic ( fun x -> x.user_name.Equals("foo") && x.password.Equals("bar"))

let sslCert = X509Certificate.FromPKCS12(BIO.File("suave.p12","r"), "easy")

let logger = Loggers.sane_defaults_for Debug

let myapp : WebPart =
  choose [
    GET >>= choose
      [ url "/hello" >>= OK "Hello GET" ; url "/goodbye" >>= OK "Good bye GET" ];
    POST >>= choose
      [ url "/hello" >>= OK "Hello POST" ; url "/goodbye" >>= OK "Good bye POST" ];
    DELETE >>= choose
      [ url "/hello" >>= OK "Hello DELETE" ; url "/goodbye" >>= OK "Good bye DELETE" ];
    PUT >>= choose
      [ url "/hello" >>= OK "Hello PUT" ; url "/goodbye" >>= OK "Good bye PUT" ];
  ]

// typed routes
let testapp : WebPart =
  choose [
    log logger log_format >>= never
    url_scan "/add/%d/%d"   (fun (a,b) -> OK((a + b).ToString()))
    url_scan "/minus/%d/%d" (fun (a,b) -> OK((a - b).ToString()))
    url_scan "/divide/%d/%d" (fun (a,b) -> OK((a / b).ToString()))
    RequestErrors.NOT_FOUND "Found no handlers"
  ]

System.Net.ServicePointManager.DefaultConnectionLimit <- Int32.MaxValue
open Socket

let timeout =
  fun r ->
  // blocking time consuming task
  Async.RunSynchronously <| async { do! Async.Sleep 10000 }
  OK "Did not timed out." r

// Adds a new mime type to the default map
let mime_types =
  Writers.default_mime_types_map
    >=> (function | ".avi" -> Writers.mk_mime_type "video/avi" false | _ -> None)

choose [
  log logger log_format >>= never
  GET >>= url "/hello" >>= never
  url_regex "(.*?)\.(dll|mdb|log)$" >>= RequestErrors.FORBIDDEN "Access denied."
  url "/neverme" >>= never >>= OK (Guid.NewGuid().ToString())
  url "/guid" >>= OK (Guid.NewGuid().ToString())
  url "/hello" >>= OK "Hello World"
  GET >>= url "/query" >>= request( fun x -> cond (x.query) ? name (fun y -> OK ("Hello " + y)) never)
  GET >>= url "/query" >>= OK "Hello beautiful"
  url "/redirect" >>= Redirection.redirect "/redirected"
  url "/redirected" >>=  OK "You have been redirected."
  url "/date" >>= warbler (fun _ -> OK (DateTimeOffset.UtcNow.ToString("o")))
  url "/timeout" >>= timeout_webpart (TimeSpan.FromSeconds 1.) timeout
  url "/session"
    >>= session_support (TimeSpan(0,30,0))
    >>= context (fun x ->
        let get,set = session x
        match get "counter" with
        | Some y ->
          set "counter" (y + 1)
          OK (sprintf "Hello %d time(s)" y )
        | None ->
          set "counter" 1
          OK "First time")
  basic_auth // from here on it will require authentication
  GET >>= url "/events" >>= request (fun r -> EventSource.hand_shake (CounterDemo.counter_demo r))
  GET >>= browse //serves file if exists
  GET >>= dir //show directory listing
  HEAD >>= url "/head" >>= fun ctx ->
      Response.async_succeed(async {
        do! Async.Sleep 1000
        // if the web parts didn't return XXX Option all the time, this row
        // could use them:
        return Response.response Codes.HTTP_200 (UTF8.bytes "Nice sleep...") ctx
      })
  POST >>= url "/upload" >>= OK "Upload successful."
  POST >>= url "/upload2"
    >>= request (fun x ->
                   let files = x.files |> Seq.fold (fun x y -> x + "<br/>" + (sprintf "(%s, %s, %s)" y.FileName y.MimeType y.Path)) ""
                   OK (sprintf "Upload successful.<br>POST data: %A<br>Uploaded files (%d): %s" (x.form)(x.files.Count) files))
  POST >>= request (fun x -> OK (sprintf "POST data: %A" (x.raw_form)))
  RequestErrors.NOT_FOUND "Found no handlers"
  ]
  |> web_server
      { bindings =
        [ HttpBinding.Create(HTTP, "127.0.0.1", 8082)
        ; { scheme = HTTPS (open_ssl sslCert); ip = IPAddress.Parse "127.0.0.1"; port = 8083us } ]
      ; error_handler    = default_error_handler
      ; listen_timeout   = TimeSpan.FromMilliseconds 2000.
      ; ct               = Async.DefaultCancellationToken
      ; buffer_size      = 2048
      ; max_ops          = 100
      ; mime_types_map   = mime_types
      ; home_folder      = None
      ; compressed_files_folder = None
      ; logger           = logger
      ; session_provider = new DefaultSessionProvider() }
