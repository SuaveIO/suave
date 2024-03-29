module Program

open System
open System.Net

open Suave
open Suave.Sockets.Control
open Suave.Logging
open Suave.Operators
open Suave.EventSource
open Suave.Filters
open Suave.Writers
open Suave.Files
open Suave.Successful
open Suave.State.CookieStateStore

let basicAuth =
  Authentication.authenticateBasic ((=) ("foo", "bar"))

// This demonstrates how to customise the console logger output.
// In most cases you wont need this. Instead you can use the more succinct:
// `let logger = Targets.create Verbose [||]`
let loggingOptions =
  { Literate.LiterateOptions.create() with
      getLogLevelText = function Verbose->"V" | Debug->"D" | Info->"I" | Warn->"W" | Error->"E" | Fatal->"F" }

let logger = LiterateConsoleTarget(
                name = [|"Suave";"Examples";"Example"|],
                minLevel = Verbose,
                options = loggingOptions,
                outputTemplate = "[{level}] {timestampUtc:o} {message} [{source}]{exceptions}"
              ) :> Logger

///  With this workflow you can write WebParts like this
let task : WebPart =
  fun ctx -> WebPart.asyncOption {
    let! ctx = GET ctx
    let! ctx = Writers.setHeader "foo" "bar" ctx
    return ctx
  }

///  we can still use the old symbol but now has a new meaning
let foo : WebPart = fun ctx -> GET ctx >>= OK "hello"

let myApp =
  choose [
    GET >=> choose
      [ path "/hello" >=> OK "Hello GET" ; path "/goodbye" >=> OK "Good bye GET" ];
    POST >=> choose
      [ path "/hello" >=> OK "Hello POST" ; path "/goodbye" >=> OK "Good bye POST" ];
    DELETE >=> choose
      [ path "/hello" >=> OK "Hello DELETE" ; path "/goodbye" >=> OK "Good bye DELETE" ];
    PUT >=> choose
      [ path "/hello" >=> OK "Hello PUT" ; path "/goodbye" >=> OK "Good bye PUT" ];
  ]

// typed routes
let testApp =
  choose [
    logStructured logger logFormatStructured >=> never
    pathScan "/add/%d/%d"   (fun (a,b) -> OK((a + b).ToString()))
    pathScan "/minus/%d/%d" (fun (a,b) -> OK((a - b).ToString()))
    pathScan "/divide/%d/%d" (fun (a,b) -> OK((a / b).ToString()))
    RequestErrors.NOT_FOUND "Found no handlers"
  ]

System.Net.ServicePointManager.DefaultConnectionLimit <- Int32.MaxValue

// How to write a new primitive WebPart
let sleep (milliseconds:int) message: WebPart =
  fun (x : HttpContext) ->
    async {
      do! Async.Sleep milliseconds
      return! OK message x
      }

// Adds a new mime type to the default map
let mimeTypes =
  Writers.defaultMimeTypesMap
    @@ (function | ".avi" -> Writers.createMimeType "video/avi" false | _ -> None)

let unzipBody : WebPart =
  fun ctx -> WebPart.asyncOption {
    if ctx.request.header "content-encoding" = Choice1Of2 "gzip" then
      return { ctx with request = { ctx.request with rawForm = Utils.Compression.gzipDecode ctx.request.rawForm} }
    else
      return ctx }

open System.IO
open Suave.Sockets
open Suave.Sockets.Control

let app =
  choose [
    GET >=> path "/hello" >=> never
    pathRegex "(.*?)\.(dll|mdb|log)$" >=> RequestErrors.FORBIDDEN "Access denied."
    path "/neverme" >=> never >=> OK (Guid.NewGuid().ToString())
    path "/guid" >=> OK (Guid.NewGuid().ToString())
    path "/hello" >=> OK "Hello World"
    path "/byte-stream" >=> (fun ctx ->

      let write (conn:Connection, _) = socket {
        use ms = new MemoryStream()
        ms.Write([| 1uy; 2uy; 3uy |], 0, 3)
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        // do things here
        do! conn.asyncWriteLn (sprintf "Content-Length: %d\r\n" ms.Length) 
        do! conn.flush()
        do! transferStream conn ms
      }

      { ctx with
          response =
            { ctx.response with
                status = HTTP_200.status
                content = SocketTask write } }
      |> succeed
    )
    (path "/apple" <|> path "/orange") >=> OK "Hello Fruit"
    GET >=> path "/query" >=> request( fun x -> cond (x.queryParam "name") (fun y -> OK ("Hello " + y)) never)
    GET >=> path "/query" >=> OK "Hello beautiful"
    GET >=> path "/error" >=> (context(fun ctx -> failwith "terror" ; OK "Do not reach this point"))
    path "/redirect" >=> Redirection.redirect "/redirected"
    path "/redirected" >=>  OK "You have been redirected."
    path "/date" >=> warbler (fun _ -> OK (DateTimeOffset.UtcNow.ToString("o")))
    path "/timeout" >=> timeoutWebPart (TimeSpan.FromSeconds 1.) (sleep 120000 "Did not timed out")
    path "/session"
      >=> statefulForSession // Session.State.CookieStateStore
      >=> context (fun x ->
        match HttpContext.state x with
        | None ->
          // restarted server without keeping the key; set key manually?
          let msg = "Server Key, Cookie Serialiser reset, or Cookie Data Corrupt, "
                    + "if you refresh the browser page, you'll have gotten a new cookie."
          OK msg

        | Some store ->
          match store.get "counter" with
          | Some y ->
            store.set "counter" (y + 1)
            >=> OK (sprintf "Hello %d time(s)" (y + 1) )
          | None ->
            store.set "counter" 1
            >=> OK "First time")
    basicAuth <| choose [ // from here on it will require authentication
        // surf to: http://localhost:8082/es.html to view the ES
        GET >=> path "/events2" >=> request (fun _ -> EventSource.handShake (fun out ->
          socket {
            let msg = { id = "1"; data = "First Message"; ``type`` = None }
            do! msg |> send out
            let msg = { id = "2"; data = "Second Message"; ``type`` = None }
            do! msg |> send out
          }))
        GET >=> path "/events" >=> request (fun r -> EventSource.handShake (CounterDemo.counterDemo r))
        GET >=> browseHome //serves file if exists
        GET >=> dirHome //show directory listing
        HEAD >=> path "/head" >=> sleep 100 "Nice sleep .."
        POST >=> path "/upload" >=> OK "Upload successful."
        POST >=> path "/i18nforms" >=> request (fun r ->
          sprintf """
          ödlan: %A
          小: %A
          """ (r.formData "ödlan") (r.formData "小")
          |> OK >=> Writers.setMimeType "text/plain"
        )
        PUT >=> path "/upload2"
          >=> request (fun x ->
             let files =
               x.files
               |> Seq.map (fun y -> sprintf "(%s, %s, %s)" y.fileName y.mimeType y.tempFilePath)
               |> String.concat "<br/>"
             OK (sprintf "Upload successful.<br>POST data: %A<br>Uploaded files (%d): %s" x.multiPartFields (List.length x.files) files))
        POST >=> request (fun x -> OK (sprintf "POST data: %s" (System.Text.Encoding.ASCII.GetString x.rawForm)))
        GET
          >=> path "/custom_header"
          >=> setHeader "X-Doge-Location" "http://www.elregalista.com/wp-content/uploads/2014/02/46263312.jpg"
          >=> OK "Doooooge"
        RequestErrors.NOT_FOUND "Found no handlers"
      ]
    ] >=> logStructured logger logFormatStructured

open System.Security.Cryptography.X509Certificates

[<EntryPoint>]
let main argv =
  startWebServer
    { bindings              = [ HttpBinding.createSimple HTTP "127.0.0.1" 8082
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
      logger                = logger
      cookieSerialiser      = new BinaryFormatterSerialiser()
      hideHeader            = false
      maxContentLength      = 1000000 }
    app
  0

(*
// using Suave.OpenSSL
// also see https://github.com/SuaveIO/suave/issues/291
// and https://github.com/exira/static-mailer/blob/72fdebf37bafc48ea7277ee4a6b2a758df5c3b3d/src/Program.fs#L28-L31
open Suave.OpenSSL
open OpenSSL.Core
open System.Security.Cryptography.X509Certificates

let cert =
  let bio = BIO.MemoryBuffer()
  let cert = System.IO.File.ReadAllBytes "example.pem"
  bio.Write cert
  OpenSSL.X509.X509Certificate.FromDER bio

Or using the built-in SSL support:
  let cert = new X509Certificate2("suave.p12","easy")
  HttpBinding.createSimple (HTTPS cert) "127.0.0.1" 8443

*)
