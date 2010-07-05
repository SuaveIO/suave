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
       //TODO: we should read shunks, less context switching
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

type HttpRequest() = 
    let mutable url : string = null
    let mutable meth0d : string = null
    let mutable stream : Stream = null
    let mutable query  : Dictionary<string,string> = new Dictionary<string,string>()
    let mutable headers: Dictionary<string,string> = new Dictionary<string,string>()
    let mutable form   : Dictionary<string,string> = new Dictionary<string,string>()
    
    let mutable username: string = null
    let mutable password: string = null
    
    member h.Url with get() = url and set x = url <- x
    member h.Method with get() = meth0d and set x = meth0d <- x
    member h.Stream with get() = stream and set x = stream <- x
    member h.Query with get() = query and set x = query <- x
    member h.Headers with get() = headers and set x = headers <- x
    member h.Form with get() = form and set x = form <- x
    
    member h.Username with get() = username and set x = username <- x
    member h.Password with get() = password and set x = password <- x

let empty_query_string () = new Dictionary<string,string>()

let parse_data (s:string) = 
    
    let param = empty_query_string ()
    let kk = s.Split('&')
    Array.iter (fun (k:string) ->
                            k.Split('=')  |> (fun d -> param.Add(d.[0],d.[1]))) kk
    param                            

let parse_url (line:string) =
    let parts = line.Split(' ')
    if(parts.Length <2 || parts.Length>3) then failwith "400"
    let indexOfMark = parts.[1].IndexOf('?')
    
    if (indexOfMark>0) then
        (parts.[0],parts.[1].Substring(0,indexOfMark),parse_data (parts.[1].Substring(indexOfMark+1)))
    else 
        (parts.[0],parts.[1],null)
    
let read_post_data (stream:Stream) (bytes : int) = 
    stream.AsyncRead(bytes)
     
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
    
    if meth.Equals("POST") then 
        let content_enconding = headers.["content-type"]
        let content_length = Convert.ToInt32(headers.["content-length"])
        let rawdata = read_post_data stream content_length  |> Async.RunSynchronously
        if content_enconding.Equals("application/x-www-form-urlencoded") then
            let form_data = parse_data (toString (rawdata, 0, rawdata.Length))
            request.Form <- form_data
            
    return request
}    

let dir s (x:HttpRequest) = if s = x.Url then Some(x) else None
let meth0d s (x:HttpRequest) = if s = x.Method then Some(x) else None

let never _ = None

let proto_version = "HTTP/1.1"

let response statusCode message (content:string) headers (http_request:HttpRequest) = async {
    do! async_writeln (http_request.Stream) (sprintf "%s %d %s" proto_version statusCode message)
    for (x,y) in headers do
        do! async_writeln (http_request.Stream) (sprintf "%s:%s" x y )
    do! async_writeln (http_request.Stream) (sprintf "Content-Type:%s" "text/html")
    let content_bytes = bytes content
    do! async_writeln (http_request.Stream) (sprintf "Content-Length:%d" (content_bytes.Length))
    do! async_writeln (http_request.Stream) ""
    do! async_writebytes (http_request.Stream) content_bytes
}    
    
let challenge  =
    response 401 "Authorization Required" "401 Unauthorized." [ ("WWW-Authenticate","Basic realm=\"protected\"")] 

let ok s  = response  200 "OK" s []  >> succeed 
let failure message = response 500 "Internal Error" message  [] >> succeed 
let redirect url  = response 302 url "Content Moved"  [] >> succeed 
let notfound message = response 404 "Not Found" message  [] >>  succeed 

let file filename  = 
    if File.Exists(filename) then
        ok (File.ReadAllText(filename)) 
    else
        response 404 "Not Found" (sprintf "%s File not found" filename)  [] >> succeed 
        
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
   
let web_server bindings webpart =
    
    bindings 
    |> Array.map (fun (proto,ip,port) ->  tcp_ip_server (ip,port) (request_loop webpart proto))
    |> Async.Parallel
    