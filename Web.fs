module Suave.Web

open Combinator

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text
open System.Diagnostics
open System.Threading
open System.Security.Permissions
open System.Security.Principal
open System.Collections.Generic

let suave_version = "0.1"

let stream (client:TcpClient) = client.GetStream()

let mirror (clientStream:Stream) (serverStream:Stream) = async {
    while true do
        let! onebyte = clientStream.AsyncRead(1)
        do! serverStream.AsyncWrite(onebyte) 
}

type TcpListener with
    member x.AsyncAcceptTcpClient() = 
        Async.FromBeginEnd(x.BeginAcceptTcpClient,x.EndAcceptTcpClient)  
        
let tcp_ip_server (sourceip,sourceport) serve_client = async {

    let server = new TcpListener(IPAddress.Parse(sourceip),sourceport)
    server.Start()

    while true do
        
        let! client = server.AsyncAcceptTcpClient() 
        serve_client client |> Async.Start
}        

let eol = "\r\n"

let bytes (s:string) = Encoding.ASCII.GetBytes(s)

let EOL = bytes eol

let rec readTillEOL(stream: Stream, buff: byte[], count: int) =
   async{
       
       let! inp = stream.AsyncRead(1)
       
       if count>0 && buff.[count-1] = EOL.[0] && inp.[0] = EOL.[1] then
          return (count-1)
       else

          buff.[count] <- inp.[0]
          return! readTillEOL(stream,buff,count + 1)
   }

let async_writeln (stream:Stream) s = async {
    let b = bytes s
    do! stream.AsyncWrite(b, 0, b.Length)
    do! stream.AsyncWrite(EOL, 0, 2)    
} 

let async_writebytes (stream:Stream) b = async {
    stream.Write(b, 0, b.Length)    
}    
    
let toString ( buff: byte[], index:int, count:int) =
    Encoding.ASCII.GetString(buff,index,count)
    
let max_buffer = 1024    
    
let read_line stream = async {
    let buf = Array.zeroCreate max_buffer 
    let! count = readTillEOL(stream,buf,0)  
    return toString(buf, 0, count)  
} 

let encode_base64 (s:string) = 
    let bytes = ASCIIEncoding.ASCII.GetBytes(s);
    Convert.ToBase64String(bytes);
    
let decode_base64 (s:string) =
     let bytes = Convert.FromBase64String(s);
     ASCIIEncoding.ASCII.GetString(bytes);

let read_headers stream = async {
    let eof = ref false
    let headers = new Dictionary<string,string>()
    while not(!eof) do
        let buf = Array.zeroCreate max_buffer
        let! count = readTillEOL(stream,buf,0)
        if count <> 0 then
            let line = toString(buf, 0, count)
            let indexOfColon = line.IndexOf(':'); 
            headers.Add (line.Substring(0,indexOfColon).ToLower(),line.Substring(indexOfColon+1).TrimStart())
        else
            eof := true
    return headers
}

let session_map = new Dictionary<string,Dictionary<string,obj>>()        
        
let session sessionId = 
    if not (session_map.ContainsKey sessionId) then
        session_map.Add(sessionId,new Dictionary<string,obj>())
    session_map.[sessionId] 
    
type HttpResponse()=
    let mutable headers: List<string*string> = new List<string*string>()
    member h.Headers with get() = headers and set x = headers <- x
 
type HttpRequest() = 
    let mutable url : string = null
    let mutable meth0d : string = null
    let mutable stream : Stream = null
    let mutable query  : Dictionary<string,string> = new Dictionary<string,string>()
    let mutable headers: Dictionary<string,string> = new Dictionary<string,string>()
    let mutable form   : Dictionary<string,string> = new Dictionary<string,string>()
    let mutable cookies   : Dictionary<string,(string*string)[]> = new Dictionary<string,(string*string)[]>()
    
    let mutable username: string = null
    let mutable password: string = null
    
    let mutable sessionId : string = null
    
    let mutable response : HttpResponse = new HttpResponse()
    
    member h.Url with get() = url and set x = url <- x
    member h.Method with get() = meth0d and set x = meth0d <- x
    member h.Stream with get() = stream and set x = stream <- x
    member h.Query with get() = query and set x = query <- x
    member h.Headers with get() = headers and set x = headers <- x
    member h.Form with get() = form and set x = form <- x
    member h.Cookies with get() = cookies and set x = cookies <- x
    
    member h.Username with get() = username and set x = username <- x
    member h.Password with get() = password and set x = password <- x
    
    member h.SessionId with get() = sessionId and set x = sessionId <- x
    
    member h.Session with get() = session sessionId
    
    member h.Response with get() = response

let empty_query_string () = new Dictionary<string,string>()

let query (x:HttpRequest) = x.Query
let form (x:HttpRequest) = x.Form

let parse_data (s:string) = 
    
    let param = empty_query_string ()
    s.Split('&')
    |> Array.iter (fun (k:string) ->
                            k.Split('=')  |> (fun d -> if d.Length = 2 then param.Add(d.[0],d.[1]))) 
    param                            

let parse_url (line:string) =
    let parts = line.Split(' ')
    if(parts.Length <2 || parts.Length>3) then failwith "400"
    let indexOfMark = parts.[1].IndexOf('?')
    
    if (indexOfMark>0) then
        (parts.[0],parts.[1].Substring(0,indexOfMark),parse_data (parts.[1].Substring(indexOfMark+1)))
    else 
        (parts.[0],parts.[1],empty_query_string ())
    
let read_post_data (stream:Stream) (bytes : int) = 
    stream.AsyncRead(bytes)
    
let parse_cookie (s:string) =
    s.Split(';') 
    |> Array.map (fun (x:string) -> 
                        let parts = x.Split('='); 
                        (parts.[0],parts.[1]))
     
let process_request (stream:Stream) = async {
    
    let request = new HttpRequest()
    request.Stream <- stream
    
    let! first_line = read_line stream 
    
    let (meth,url,query) as q = parse_url (first_line)
    request.Url <- url
    request.Method <- meth
    request.Query <- query
    
    let! headers = read_headers stream 
    
    request.Headers <- headers
    
    request.Headers 
    |> Seq.filter (fun x -> x.Key.Equals("cookie"))
    |> Seq.iter (fun x -> 
                    let cookie = parse_cookie x.Value; 
                    request.Cookies.Add (fst(cookie.[0]),cookie))
    
    if meth.Equals("POST") then 
        let content_enconding = headers.["content-type"]
        let content_length = Convert.ToInt32(headers.["content-length"])
        let rawdata = read_post_data stream content_length  |> Async.RunSynchronously
        if content_enconding.Equals("application/x-www-form-urlencoded") then
            let form_data = parse_data (toString (rawdata, 0, rawdata.Length))
            request.Form <- form_data
            
    return request
} 

let set_header a b (http_request:HttpRequest) = 
    http_request.Response.Headers.Add(a,b)
    http_request
    
let set_cookie cookie = set_header  "Set-Cookie"  cookie 

//cookie-based session support   
let session_support (request:HttpRequest) =
    let sessionId = 
        match request.Cookies ? suave_session_id with
        |Some(attr) -> snd(attr.[0])
        |None -> Guid.NewGuid().ToString()
    request.SessionId <- sessionId
    set_cookie (sprintf "%s=%s" "suave_session_id" sessionId) request |> ignore
    Some(request)
        
let url s (x:HttpRequest) = if s = x.Url then Some(x) else None
let meth0d s (x:HttpRequest) = if s = x.Method then Some(x) else None

let never _ = None

let proto_version = "HTTP/1.1"

let response statusCode message (content:string) (http_request:HttpRequest) = async {

    do! async_writeln (http_request.Stream) (sprintf "%s %d %s" proto_version statusCode message)
    do! async_writeln (http_request.Stream) (sprintf "Server: Suave/%s" suave_version)
    do! async_writeln (http_request.Stream) (sprintf "Date: %s" (DateTime.Now.ToUniversalTime().ToString("R")))
    
    for (x,y) in http_request.Response.Headers do
        do! async_writeln (http_request.Stream) (sprintf "%s: %s" x y )
    
    if not(http_request.Response.Headers.Exists(new Predicate<_>(fun (x,_) -> x.ToLower().Equals("content-type")))) then
        do! async_writeln (http_request.Stream) (sprintf "Content-Type: %s" "text/html")
    
    let content_bytes = bytes content
    
    if content_bytes.Length > 0 then 
        do! async_writeln (http_request.Stream) (sprintf "Content-Length: %d" (content_bytes.Length))
        
    do! async_writeln (http_request.Stream) ""
    
    if content_bytes.Length > 0 then
        do! async_writebytes (http_request.Stream) content_bytes
}

let challenge  =
    set_header "WWW-Authenticate" "Basic realm=\"protected\"" 
    >> response 401 "Authorization Required" "401 Unauthorized." 

let ok s  = response  200 "OK" s >> succeed 
let failure message = response 500 "Internal Error" message >> succeed 

let redirect url  = 
    set_header "Location" url
    >> response 302 url "Content Moved"
    >> succeed 

let notfound message = response 404 "Not Found" message >>  succeed 

let mime_type = function
    |".bmp" -> "image/bmp"
    |".css" -> "text/css"
    |".gif" -> "image/gif"
    |".ico" -> "image/x-icon"
    |".htm" 
    |".html" -> "text/html";
    |".jpe" 
    |".jpeg"
    |".jpg" -> "image/jpeg"
    |".js" -> "application/x-javascript"
    |".exe" -> "application/exe"
    |_ -> "application/octet-streamu"

let file filename  = 
    if File.Exists(filename) then
        let file_info = new FileInfo(filename)
        let mimes = mime_type (file_info.Extension)
        set_header "Content-Type" mimes  >> ok (File.ReadAllText(filename)) 
    else
        response 404 "Not Found" (sprintf "%s File not found" filename) >> succeed 

let local_file str = sprintf "%s%s" Environment.CurrentDirectory str        
        
let browse (http_request:HttpRequest) = 
    let filename = local_file http_request.Url
    file filename http_request     

let closepipe (p:HttpRequest option) =
    match p with
    |Some(x) -> x.Stream.Flush(); x.Stream.Close()
    |None -> ()
    
let parse_authentication_token (token:string) =
    let parts = token.Split (' ')
    let enc = parts.[1].Trim()
    let decoded = decode_base64 enc
    let indexOfColon = decoded.IndexOf(':')
    (parts.[0].ToLower(),decoded.Substring(0,indexOfColon),decoded.Substring(indexOfColon+1))    
    
let authenticate_basic f (p:HttpRequest) =
    let headers = p.Headers
    if headers.ContainsKey("authorization") then
        let header = headers.["authorization"]
        let (typ,username,password) =  parse_authentication_token header
        p.Username <- username
        p.Password <- password
        if (typ.Equals("basic")) && f p then
            fail
        else
            challenge p |> succeed 
    else
        challenge p |> succeed 
        
let log (s:Stream) (http_request:HttpRequest)  = 
    let bytes = bytes (sprintf "%A\n" (http_request.Method,http_request.Url,http_request.Query, http_request.Form, http_request.Headers))
    s.Write(bytes,0,bytes.Length)
    succeed http_request

open System.Net    
open System.Net.Security
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates;

type Protocols = | HTTP | HTTPS of X509Certificate
    
let load_stream proto (stream: Stream)  = 
    match proto with
    |HTTP -> stream 
    |HTTPS(cert) -> 
                let sslStream = new SslStream(stream, true) 
                sslStream.AuthenticateAsServer(cert, false, SslProtocols.Default, true)
                sslStream :> Stream;

let request_loop webpart proto (client:TcpClient) = async {
    
    try
    
        let keep_alive = ref true
        use stream = 
            client.GetStream()
            |> load_stream proto
            
        while !keep_alive do
            let! request = process_request stream
            let p = webpart request
            match p with 
            |Some(x) -> do! x
            |None -> ()
            stream.Flush()
            keep_alive := request.Headers ? connection .Equals("keep-alive")
            
        client.Close()
    with |x -> printf "client disconnected %A" x
}
    
let parallelize input f = input |> Seq.map (f)  |> Async.Parallel

type Binding = Protocols * string * int      
type WebPart = HttpRequest -> Async<unit> Option
   
let web_server bindings (webpart:WebPart) =
    
    bindings 
    |> Array.map (fun (proto,ip,port) ->  tcp_ip_server (ip,port) (request_loop webpart proto))
    |> Async.Parallel
    