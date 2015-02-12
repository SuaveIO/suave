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

let basicAuth =
  Authentication.authenticateBasic ((=) ("foo", "bar"))

let logger = Loggers.ConsoleWindowLogger LogLevel.Verbose

let myApp =
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
let testApp =
  choose [
    log logger logFormat >>= never
    urlScan "/add/%d/%d"   (fun (a,b) -> OK((a + b).ToString()))
    urlScan "/minus/%d/%d" (fun (a,b) -> OK((a - b).ToString()))
    urlScan "/divide/%d/%d" (fun (a,b) -> OK((a / b).ToString()))
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
    GET >>= url "/hello" >>= never
    urlRegex "(.*?)\.(dll|mdb|log)$" >>= RequestErrors.FORBIDDEN "Access denied."
    url "/neverme" >>= never >>= OK (Guid.NewGuid().ToString())
    url "/guid" >>= OK (Guid.NewGuid().ToString())
    url "/hello" >>= OK "Hello World"
    (url "/apple" <|> url "/orange") >>= OK "Hello Fruit"
    GET >>= url "/query" >>= request( fun x -> cond (x.queryParam "name") (fun y -> OK ("Hello " + y)) never)
    GET >>= url "/query" >>= OK "Hello beautiful"
    url "/redirect" >>= Redirection.redirect "/redirected"
    url "/redirected" >>=  OK "You have been redirected."
    url "/date" >>= warbler (fun _ -> OK (DateTimeOffset.UtcNow.ToString("o")))
    url "/timeout" >>= timeoutWebPart (TimeSpan.FromSeconds 1.) (sleep 120000 "Did not timed out")
    url "/session"
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
    GET >>= url "/events" >>= request (fun r -> EventSource.handShake (CounterDemo.counterDemo r))
    GET >>= browseHomeDirectory //serves file if exists
    GET >>= dirHomeDirectory //show directory listing
    HEAD >>= url "/head" >>= sleep 100 "Nice sleep .."
    POST >>= url "/upload" >>= OK "Upload successful."
    PUT >>= url "/upload2"
      >>= request (fun x ->
         let files =
           x.files 
           |> Seq.map (fun y -> sprintf "(%s, %s, %s)" y.fileName y.mimeType y.tempFilePath)
           |> String.concat "<br/>"
         OK (sprintf "Upload successful.<br>POST data: %A<br>Uploaded files (%d): %s" x.multiPartFields (List.length x.files) files))
    POST >>= request (fun x -> OK (sprintf "POST data: %s" (System.Text.Encoding.ASCII.GetString x.rawForm)))
    GET
      >>= url "/custom_header"
      >>= setHeader "X-Doge-Location" "http://www.elregalista.com/wp-content/uploads/2014/02/46263312.jpg"
      >>= OK "Doooooge"
    RequestErrors.NOT_FOUND "Found no handlers"
    ] >>= log logger logFormat

[<EntryPoint>]
let main argv =
  startWebServer
    { bindings         = [ HttpBinding.mk' HTTP "127.0.0.1" 8082 ]
      serverKey       = Utils.Crypto.generateKey HttpRuntime.ServerKeyLength
      errorHandler    = defaultErrorHandler
      listenTimeout   = TimeSpan.FromMilliseconds 2000.
      cancellationToken = Async.DefaultCancellationToken
      bufferSize      = 2048
      maxOps          = 100
      mimeTypesMap   = mimeTypes
      homeFolder      = None
      compressedFilesFolder = None
      logger           = logger }
    app
  0