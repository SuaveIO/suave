module Program

open System
open System.Net

open Suave
open Suave.Logging
open Suave.Web
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Writers
open Suave.Http.Files
open Suave.Http.Successful
open Suave.Types
open Suave.State.CookieStateStore
open Suave.Utils

let basic_auth : WebPart =
  Authentication.authenticate_basic ( fun (user_name,password) -> user_name.Equals("foo") && password.Equals("bar"))

let logger = Loggers.ConsoleWindowLogger LogLevel.Verbose

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

let sleep milliseconds message: WebPart =
  fun (x : HttpContext) ->
    async {
      do! Async.Sleep milliseconds
      return! OK message x
      }

// Adds a new mime type to the default map
let mime_types =
  Writers.default_mime_types_map 
  |> Map.add (".avi", MimeType.mk "video/avi" false)

let app =
  choose [
    GET >>= url "/hello" >>= never
    url_regex "(.*?)\.(dll|mdb|log)$" >>= RequestErrors.FORBIDDEN "Access denied."
    url "/neverme" >>= never >>= OK (Guid.NewGuid().ToString())
    url "/guid" >>= OK (Guid.NewGuid().ToString())
    url "/hello" >>= OK "Hello World"
    (url "/apple" <|> url "/orange") >>= OK "Hello Fruit"
    GET >>= url "/query" >>= request( fun x -> cond ((HttpRequest.query x) ^^ "name") (fun y -> OK ("Hello " + y)) never)
    GET >>= url "/query" >>= OK "Hello beautiful"
    url "/redirect" >>= Redirection.redirect "/redirected"
    url "/redirected" >>=  OK "You have been redirected."
    url "/date" >>= warbler (fun _ -> OK (DateTimeOffset.UtcNow.ToString("o")))
    url "/timeout" >>= timeout_webpart (TimeSpan.FromSeconds 1.) (sleep 120000 "Did not timed out")
    url "/session"
      >>= stateful' // Session.State.CookieStateStore
      >>= context (fun x ->
        match x |> HttpContext.state with
        | None -> Redirection.FOUND "/session" // restarted server without keeping the key; set key manually?
        | Some store ->
          match store.get "counter" with
          | Some y ->
            store.set "counter" (y + 1)
            >>= OK (sprintf "Hello %d time(s)" y )
          | None ->
            store.set "counter" 1
            >>= OK "First time")
    basic_auth // from here on it will require authentication
    GET >>= url "/events" >>= request (fun r -> EventSource.hand_shake (CounterDemo.counter_demo r))
    GET >>= browse' //serves file if exists
    GET >>= dir' //show directory listing
    HEAD >>= url "/head" >>= sleep 100 "Nice sleep .."
    POST >>= url "/upload" >>= OK "Upload successful."
    PUT >>= url "/upload2" >>= ParsingAndControl.parse_post_data
      >>= request (fun x ->
         let files =
           x.files
           |> Seq.fold
             (fun x y -> x + "<br/>" + (sprintf "(%s, %s, %s)" y.file_name y.mime_type y.temp_file_path))
             ""
         OK (sprintf "Upload successful.<br>POST data: %A<br>Uploaded files (%d): %s" x.multipart_fields (List.length x.files) files))
    POST >>= ParsingAndControl.parse_post_data >>= request (fun x -> OK (sprintf "POST data: %s" (ASCII.to_string' x.raw_form)))
    GET
      >>= url "/custom_header"
      >>= set_header "X-Doge-Location" "http://www.elregalista.com/wp-content/uploads/2014/02/46263312.jpg"
      >>= OK "Doooooge"
    RequestErrors.NOT_FOUND "Found no handlers"
    ] >>= log logger log_format

[<EntryPoint>]
let main argv =
  web_server
    { props =
        { bindings         = [ HttpBinding.mk' HTTP "127.0.0.1" 8082 ]
          server_key       = Utils.Crypto.generate_key HttpRuntime.ServerKeyLength
          listen_timeout   = TimeSpan.FromMilliseconds 2000.
          buffer_size      = 2048
          max_ops          = 100
          mime_types_map   = mime_types
          home_folder      = None
          compressed_files_folder = None }
      error_handler    = default_error_handler
      ct               = Async.DefaultCancellationToken
      logger           = logger }
    app
  0