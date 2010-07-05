
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
    url "/hello" >>= url "hello" >>= ok "Hello World";
    url "/query" >>= warbler( fun x -> ok ("Hello " + (x.Query) ? name));
    url "/redirect" >>= redirect "/date"
    url "/date"  >>= warbler( fun x -> ok (DateTime.Now.ToString()));
    basic_auth; // from here on it will require authentication
    meth0d "GET" >>= choose [ url "/template.xml" >>= process_template data ;  ];
    meth0d "GET" >>= browse
    meth0d "POST" >>= warbler( fun x -> ok (sprintf "POST data: %A" (x.Form)));
    notfound "Found no handlers"     
    ] 
    |> web_server [|HTTP,"127.0.0.1",80; HTTPS(sslCert),"192.168.13.138",443|]
    |> Async.RunSynchronously
    |> ignore