
open System
open System.Security.Cryptography.X509Certificates;
open System.Xml

open Suave.Web
open Suave.Combinator
open Suave.Template
open Suave.Html

let basic_auth  = authenticate_basic ( fun x -> x.Username.Equals("foo") && x.Password.Equals("bar"))

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
        
choose [
    Console.OpenStandardOutput() |> log >>= never ; 
    meth0d "GET" >>= url "/hello" >>= never;
    url "/hello" >>= never >>= ok "Never executes";
    url "/hello" >>= ok "Hello World"  ;
    meth0d "GET" >>= url "/query" >>= warbler( fun x -> cond (x.Query) ? name (fun y -> ok ("Hello " + y)) never);
    meth0d "GET" >>= url "/query" >>= ok "Hello beautiful";
    url "/redirect" >>= redirect "/redirected"
    url "/redirected" >>=  ok "You have been redirected.";
    url "/date" >>= ok (DateTime.Now.ToString());
    url "/session" 
        >>= session_support 
        >>= warbler ( fun x -> 
            cond (x.Session) ? counter 
                ( fun y -> 
                    x.Session ? counter <- (y :?> int) + 1 :> obj ; 
                    ok (sprintf "Hello %A time(s)"  y))
                 (x.Session ? counter <- 1 :> obj ; ok "First time"));
    basic_auth; // from here on it will require authentication
    meth0d "GET" >>= choose [ url "/lift.xml" >>= process_template data;  ];
    meth0d "GET" >>= browse ;
    meth0d "POST" >>= url "/upload" >>= ok "Upload successful.";
    meth0d "POST" >>= url "/upload2" 
        >>= warbler( fun x -> 
                        let files = x.Files |> Seq.fold (fun x y -> x + "<br>" + (sprintf "(%s,%s,%s)" y.FileName y.MimeType y.Path)) "" ;
                        ok (sprintf "Upload successful.<br>POST data: %A<br>Uploaded files (%d): %s" (x.Form)(x.Files.Count) files));
    meth0d "POST" >>= warbler( fun x -> ok (sprintf "POST data: %A" (x.Form)));
    notfound "Found no handlers"     
    ] 
    |> web_server [|HTTP,"127.0.0.1",80; HTTPS(sslCert),"127.0.0.1",443|]
    |> Async.RunSynchronously
    |> ignore