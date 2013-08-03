
open System
open System.Security.Cryptography.X509Certificates;
open System.Xml

open Suave
open Suave.Web
open Suave.Template
open Suave.Html
open Suave.Http

let basic_auth  : WebPart  = authenticate_basic ( fun x -> x.Username.Equals("foo") && x.Password.Equals("bar"))

type User() = 
    let mutable firstName = String.Empty
    member u.FirstName 
        with get() = firstName   
        and set x= firstName <- x    

let pepe = new User()
pepe.FirstName <- "Pepe"

let tags = ["science";"art";"religion"]

let data = Map["FirstName", Suave.Html.text (pepe.FirstName, fun (x:obj) -> pepe.FirstName <- x :?> string) ]

type LoginForm() = 

    let user = new User()
    
    let do_something_with user = ()  
      
    member m.init(node:Xml)  =
    
        let save_results _ = do_something_with user
        
        user.FirstName <- "Pepe"

        bind (  "h", node, 
               Map [ "FirstName", Suave.Html.text_box (user.FirstName, fun x -> user.FirstName <- x :?> string);
                     "Submit"   , Suave.Html.submit ("Submit data", save_results );  
                     "tags"     , fun xml -> tags |> flatMap (fun t -> bind ("t", xml , Map [ "name", Suave.Html.text (t, ignore)]))
               ]);
         
    member m.tags(node:Xml) = 
        
        tags |> flatMap (fun x -> bind ("t",node, Map [ "name", Suave.Html.text(x,ignore)]))
        
let sslCert = new X509Certificate("suave.pfx","easy");

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
        urlscan "/add/%d/%d" (fun (a,b) -> OK((a + b).ToString()))
        urlscan "/minus/%d/%d" (fun (a,b) -> OK((a - b).ToString()))
        notfound "Found no handlers" 
    ]

System.Net.ServicePointManager.DefaultConnectionLimit <- Int32.MaxValue
        
choose [
    Console.OpenStandardOutput() |> log >>= never ; 
    GET >>= url "/hello" >>= never;
    url "/hello" >>= never >>= OK "Never executes" ;
    url "/hello" >>= OK "Hello World"   ;
    GET >>= url "/query" >>= warbler( fun x -> cond (x.Query) ? name (fun y -> OK ("Hello " + y)) never);
    GET >>= url "/query" >>= OK "Hello beautiful" ;
    url "/redirect" >>= redirect "/redirected"
    url "/redirected" >>=  OK "You have been redirected." ;
    url "/date" >>= OK (DateTime.Now.ToString());
    url "/session" 
        >>= session_support 
        >>= warbler ( fun x -> 
            cond (x.Session) ? counter 
                ( fun y -> 
                    x.Session ? counter <- (y :?> int) + 1 :> obj ; 
                    OK (sprintf "Hello %A time(s)"  y ))
                 (x.Session ? counter <- 1 :> obj ; OK "First time" ));
    basic_auth; // from here on it will require authentication
    GET >>= choose [ url "/lift.xml" >>= process_template data;  ];
    GET >>= browse;//serves file if exists
    GET >>= dir;//show directory listing
    POST >>= url "/upload" >>= OK "Upload successful." ;
    POST >>= url "/upload2" 
        >>= warbler( fun x -> 
                        let files = x.Files |> Seq.fold (fun x y -> x + "<br>" + (sprintf "(%s,%s,%s)" y.FileName y.MimeType y.Path)) "" ;
                        OK (sprintf "Upload successful.<br>POST data: %A<br>Uploaded files (%d): %s" (x.Form)(x.Files.Count) files));
    POST >>= warbler( fun x -> OK (sprintf "POST data: %A" (x.Form)));
    notfound "Found no handlers" 
    ] 
    |> web_server [|HTTP,"127.0.0.1",80; HTTPS(sslCert),"127.0.0.1",443|]
    |> Async.RunSynchronously
    |> ignore
