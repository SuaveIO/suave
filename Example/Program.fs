
open System
open System.Net

open Suave.Web
open Suave.Http
open Suave.Types
open Suave.Session

open OpenSSL.X509
open OpenSSL.Core

let basic_auth  : WebPart =
  authenticate_basic ( fun x -> x.user_name.Equals("foo") && x.password.Equals("bar"))

let sslCert = X509Certificate.FromPKCS12(BIO.File("suave.p12","r"), "easy")

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
    Console.OpenStandardOutput() |> log >>= never ; 
    url_scan "/add/%d/%d"   (fun (a,b) -> OK((a + b).ToString()))
    url_scan "/minus/%d/%d" (fun (a,b) -> OK((a - b).ToString()))
    NOT_FOUND "Found no handlers"
  ]

System.Net.ServicePointManager.DefaultConnectionLimit <- Int32.MaxValue

let timeout r =
  async {
    do! Async.Sleep 1500
  } |> succeed

choose [
  Console.OpenStandardOutput() |> log >>= never ;

  GET >>= url "/hello" >>= never ;
  url_regex "(.*?)\.(dll|mdb|log)$" >>= FORBIDDEN "Access denied."
  url "/neverme" >>= never >>= OK (Guid.NewGuid().ToString()) ;
  url "/guid" >>= OK (Guid.NewGuid().ToString()) ;
  url "/hello" >>= OK "Hello World" ;
  GET >>= url "/query" >>= warbler( fun x -> cond (x.query) ? name (fun y -> OK ("Hello " + y)) never) ;
  GET >>= url "/query" >>= OK "Hello beautiful" ;
  url "/redirect" >>= redirect "/redirected"
  url "/redirected" >>=  OK "You have been redirected." ;
  url "/date" >>= warbler (fun _ -> OK (DateTime.Now.ToString()));
  url "/timeout" >>= timeout;
  url "/session"
    >>= session_support
    // alternative to warbler
    >>== (fun x ->
      cond (session x) ? counter
          ( fun y ->
              (session x) ? counter <- (y :?> int) + 1 :> obj ;
              OK (sprintf "Hello %A time(s)"  y ))
            ((session x) ? counter <- 1 :> obj ; OK "First time" )) ;
  basic_auth; // from here on it will require authentication
  GET >>= browse; //serves file if exists
  GET >>= dir; //show directory listing
  POST >>= url "/upload" >>= OK "Upload successful." ;
  POST >>= url "/upload2"
      >>= warbler(fun x ->
                    let files = x.files |> Seq.fold (fun x y -> x + "<br>" + (sprintf "(%s,%s,%s)" y.FileName y.MimeType y.Path)) "" ;
                    OK (sprintf "Upload successful.<br>POST data: %A<br>Uploaded files (%d): %s" (x.form)(x.files.Count) files)) ;
  POST >>= warbler( fun x -> OK (sprintf "POST data: %A" (x.raw_form)));
  NOT_FOUND "Found no handlers"
  ]
  |> web_server
      { bindings =
        [ HttpBinding.Create(HTTP, "127.0.0.1", 8082)
        ; { scheme = HTTPS(sslCert); ip = IPAddress.Parse "127.0.0.1"; port = 8083us } ]
      ; error_handler    = default_error_handler
      ; web_part_timeout = TimeSpan.FromMilliseconds 1000.
      ; listen_timeout   = TimeSpan.FromMilliseconds 2000.
      ; ct               = Async.DefaultCancellationToken }
