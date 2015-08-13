module Program

open System
open System.Net

open Suave
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Logging
open Suave.Web
open Suave.Http
open Suave.Http.EventSource
open Suave.Http.Applicatives
open Suave.Http.Writers
open Suave.Http.Files
open Suave.Http.Successful
open Suave.Types
open Suave.State.CookieStateStore
open Suave.Utils

let basicAuth =
  Authentication.authenticateBasic ((=) ("foo", "bar"))

let logger = Loggers.ConsoleWindowLogger LogLevel.Verbose

let myApp =
  choose [
    GET >>= choose
      [ path "/hello" >>= OK "Hello GET" ; path "/goodbye" >>= OK "Good bye GET" ];
    POST >>= choose
      [ path "/hello" >>= OK "Hello POST" ; path "/goodbye" >>= OK "Good bye POST" ];
    DELETE >>= choose
      [ path "/hello" >>= OK "Hello DELETE" ; path "/goodbye" >>= OK "Good bye DELETE" ];
    PUT >>= choose
      [ path "/hello" >>= OK "Hello PUT" ; path "/goodbye" >>= OK "Good bye PUT" ];
  ]

// typed routes
let testApp =
  choose [
    log logger logFormat >>= never
    pathScan "/add/%d/%d"   (fun (a,b) -> OK((a + b).ToString()))
    pathScan "/minus/%d/%d" (fun (a,b) -> OK((a - b).ToString()))
    pathScan "/divide/%d/%d" (fun (a,b) -> OK((a / b).ToString()))
    RequestErrors.NOT_FOUND "Found no handlers"
  ]

System.Net.ServicePointManager.DefaultConnectionLimit <- Int32.MaxValue

// How to write a new primitive WebPart
let sleep milliseconds message: WebPart =
  fun (x : HttpContext) ->
    async {
      do! Async.Sleep milliseconds
      return! OK message x
      }

// Adds a new mime type to the default map
let mimeTypes =
  Writers.defaultMimeTypesMap
    >=> (function | ".avi" -> Writers.mkMimeType "video/avi" false | _ -> None)

let app =
  choose [
    GET >>= path "/hello" >>= never
    pathRegex "(.*?)\.(dll|mdb|log)$" >>= RequestErrors.FORBIDDEN "Access denied."
    path "/neverme" >>= never >>= OK (Guid.NewGuid().ToString())
    path "/guid" >>= OK (Guid.NewGuid().ToString())
    path "/hello" >>= OK "Hello World"
    (path "/apple" <|> path "/orange") >>= OK "Hello Fruit"
    GET >>= path "/query" >>= request( fun x -> cond (x.queryParam "name") (fun y -> OK ("Hello " + y)) never)
    GET >>= path "/query" >>= OK "Hello beautiful"
    path "/redirect" >>= Redirection.redirect "/redirected"
    path "/redirected" >>=  OK "You have been redirected."
    path "/date" >>= warbler (fun _ -> OK (DateTimeOffset.UtcNow.ToString("o")))
    path "/timeout" >>= timeoutWebPart (TimeSpan.FromSeconds 1.) (sleep 120000 "Did not timed out")
    path "/session"
      >>= statefulForSession // Session.State.CookieStateStore
      >>= context (fun x ->
        match x |> HttpContext.state with
        | None -> Redirection.FOUND "/session" // restarted server without keeping the key; set key manually?
        | Some store ->
          match store.get "counter" with
          | Some y ->
            store.set "counter" (y + 1)
            >>= OK (sprintf "Hello %d time(s)" (y + 1) )
          | None ->
            store.set "counter" 1
            >>= OK "First time")
    basicAuth // from here on it will require authentication
    // surf to: http://localhost:8082/es.html to view the ES
    GET >>= path "/events2" >>= request (fun _ -> EventSource.handShake (fun out ->
      socket {
        let msg = { id = "1"; data = "First Message"; ``type`` = None }
        do! msg |> send out
        let msg = { id = "2"; data = "Second Message"; ``type`` = None }
        do! msg |> send out
      }))
    GET >>= path "/events" >>= request (fun r -> EventSource.handShake (CounterDemo.counterDemo r))

    GET >>= browseHome //serves file if exists
    GET >>= dirHome //show directory listing
    HEAD >>= path "/head" >>= sleep 100 "Nice sleep .."
    POST >>= path "/upload" >>= OK "Upload successful."
    POST >>= path "/i18nforms" >>= request (fun r ->
      sprintf """
      ödlan: %A
      小: %A
      """ (r.formData "ödlan") (r.formData "小")
      |> OK >>= Writers.setMimeType "text/plain"
    )
    PUT >>= path "/upload2"
      >>= request (fun x ->
         let files =
           x.files 
           |> Seq.map (fun y -> sprintf "(%s, %s, %s)" y.fileName y.mimeType y.tempFilePath)
           |> String.concat "<br/>"
         OK (sprintf "Upload successful.<br>POST data: %A<br>Uploaded files (%d): %s" x.multiPartFields (List.length x.files) files))
    POST >>= request (fun x -> OK (sprintf "POST data: %s" (System.Text.Encoding.ASCII.GetString x.rawForm)))
    GET
      >>= path "/custom_header"
      >>= setHeader "X-Doge-Location" "http://www.elregalista.com/wp-content/uploads/2014/02/46263312.jpg"
      >>= OK "Doooooge"
    RequestErrors.NOT_FOUND "Found no handlers"
    ] >>= log logger logFormat

(*open Suave.OpenSSL
open OpenSSL.Core
open System.Security.Cryptography.X509Certificates*)

[<EntryPoint>]
let main argv =
  (*let cert =
    let bio = BIO.MemoryBuffer()
    let cert = System.IO.File.ReadAllBytes "example.pem"
    bio.Write cert
    OpenSSL.X509.X509Certificate.FromDER bio*)

  startWebServer
    { bindings              = [ HttpBinding.mk' HTTP "127.0.0.1" 8082
                                //HttpBinding.mk' (HTTPS (Provider.open_ssl cert)) "127.0.0.1" 8443
                              ]
      serverKey             = Utils.Crypto.generateKey HttpRuntime.ServerKeyLength
      errorHandler          = defaultErrorHandler
      listenTimeout         = TimeSpan.FromMilliseconds 2000.
      cancellationToken     = Async.DefaultCancellationToken
      bufferSize            = 2048
      maxOps                = 100
      mimeTypesMap          = mimeTypes
      homeFolder            = None
      compressedFilesFolder = None
      logger                = logger }
    app
  0