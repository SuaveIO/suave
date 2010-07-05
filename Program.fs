
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
    meth0d "GET" >>= dir "/hello" >>= never;
    dir "/hello" >>= never >>= ok "Never executes";
    dir "/hello" >>= ok "Hello World"  ;
    dir "/hello" >>= dir "hello" >>= ok "Hello World";
    dir "/query" >>= warbler( fun x -> ok ("Hello " + (x.Query) ? name));
    dir "/date"  >>= warbler( fun x -> ok (DateTime.Now.ToString()));
    basic_auth; // from here on it will require authentication
    meth0d "GET" >>= choose [ dir "/template.xml" >>= process_template data ;  ];
    meth0d "GET" >>= browse
    meth0d "POST" >>= warbler( fun x -> ok (sprintf "POST data: %A" (x.Form)));
    notfound "Found no handlers"     
    ] 
    |> web_server [|HTTP,"127.0.0.1",80; HTTPS(sslCert),"192.168.13.146",443|]
    |> Async.RunSynchronously
    |> ignore