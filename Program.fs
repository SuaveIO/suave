
open System
open System.Security.Cryptography.X509Certificates;

open Suave.Web
open Suave.Combinator
open Suave.Template

let basic_auth  = authenticate_basic ( fun x -> x.Username.Equals("foo") && x.Password.Equals("bar"))

let user x y z = Map<string,obj> [ "FirstName" , x; "LastName", y; "Age" , z ]

let authors = 
    [ 
        user "Peter" "Hamilton" 33; 
        user "Isaac" "Simov" 44;
        user "Frank" "Herbert" 55;
    ] 
    |> List.toSeq 
    
let data = Map [ "mysource", authors ] 

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
    meth0d "GET" >>= choose [ url "/template.xml" >>= process_template data ;  ];
    meth0d "GET" >>= browse
    meth0d "POST" >>= warbler( fun x -> ok (sprintf "POST data: %A" (x.Form)));
    notfound "Found no handlers"     
    ] 
    |> web_server [|HTTP,"127.0.0.1",80; HTTPS(sslCert),"192.168.13.138",443|]
    |> Async.RunSynchronously
    |> ignore
    
web_server [|HTTP, "127.0.0.1",80|] (ok "Hello World")
|> Async.RunSynchronously
|> ignore;    


let simple_app : WebPart = url "/hello" >>= ok "Hello World" ;