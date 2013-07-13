module Suave.Web

open Utils

open System
open System.IO

open System.Text
open System.Diagnostics
open System.Threading
open System.Security.Permissions
open System.Security.Principal
open System.Collections.Generic

let suave_version = "0.1"
let proto_version = "HTTP/1.1"
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

let read_until (marker:byte array) f (stream:Stream)  = 
    
    let buffer = Array.create marker.Length (byte(0))
    
    let rec loop index = async {
        if index < Array.length marker then 
            let! byte = stream.AsyncRead(1)
            if byte.Length > 0 then 
                buffer.[index] <- byte.[0]
                if byte.[0] = marker.[index] then
                    do! loop (index + 1)  
                else 
                    do! f buffer (index+1)
                    do! loop 0
    }
    loop 0    

let async_writeln (stream:Stream) s = async {
    let b = bytes s
    do! stream.AsyncWrite(b, 0, b.Length)
    do! stream.AsyncWrite(EOL, 0, 2)    
} 

let async_writebytes (stream:Stream) b = async {
    do! stream.AsyncWrite(b, 0, b.Length)    
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

open System.Collections.Concurrent

let session_map = new Dictionary<string,ConcurrentDictionary<string,obj>>()        
        
let session sessionId = 
    if not (session_map.ContainsKey sessionId) then
        session_map.Add(sessionId,new ConcurrentDictionary<string,obj>())
    session_map.[sessionId] 
    
type HttpResponse()=
    let mutable headers: List<string*string> = new List<string*string>()
    member h.Headers with get() = headers and set x = headers <- x
    
type HttpUpload(fieldname,filename,mime_type,temp_file_name) =
    member x.FieldName = fieldname
    member x.FileName  = filename
    member x.MimeType  = mime_type
    member x.Path = temp_file_name
    
type HttpRequest() = 
    let mutable url : string = null
    let mutable meth0d : string = null
    let mutable remoteAddress : string = null
    let mutable stream : Stream = null
    let mutable query  : Dictionary<string,string> = new Dictionary<string,string>()
    let mutable headers: Dictionary<string,string> = new Dictionary<string,string>()
    let mutable form   : Dictionary<string,string> = new Dictionary<string,string>()
    let mutable rawform   : byte[] = Array.empty;
    let mutable rawquery  : string = null;
    let mutable cookies   : Dictionary<string,(string*string)[]> = new Dictionary<string,(string*string)[]>()
    let mutable username: string = null
    let mutable password: string = null
    let mutable sessionId : string = null
    let mutable response : HttpResponse = new HttpResponse()
    let mutable files  : List<HttpUpload> = new List<HttpUpload>()
    
    member h.Url with get() = url and set x = url <- x
    member h.Method with get() = meth0d and set x = meth0d <- x
    member h.RemoteAddress with get() = remoteAddress and set x = remoteAddress <- x
    member h.Stream with get() = stream and set x = stream <- x
    member h.Query with get() = query and set x = query <- x
    member h.Headers with get() = headers and set x = headers <- x
    member h.Form with get() = form and set x = form <- x
    member h.RawForm with get() = rawform and set x = rawform <- x
    member h.RawQuery with get() = rawquery and set x = rawquery <- x
    member h.Cookies with get() = cookies and set x = cookies <- x
    member h.Username with get() = username and set x = username <- x
    member h.Password with get() = password and set x = password <- x
    member h.SessionId with get() = sessionId and set x = sessionId <- x
    member h.Session with get() = session sessionId
    member h.Response with get() = response
    member h.Files with get() = files
    
    member h.Dispose(disposing:bool) =
        
        if disposing then
            GC.SuppressFinalize(h)
        
        for upload in h.Files do
            if File.Exists(upload.Path) then
                try
                    File.Delete(upload.Path)
                with
                |_ -> () // we tried
            
    override h.Finalize () = h.Dispose(false)           
    
    interface IDisposable with
        member h.Dispose() =
            h.Dispose(true)

let empty_query_string () = new Dictionary<string,string>()

let query (x:HttpRequest) = x.Query
let form (x:HttpRequest) = x.Form

let parse_data (s:string) = 
    
    let param = empty_query_string ()
    s.Split('&')
    |> Array.iter 
        (fun (k:string) -> k.Split('=')  
                            |> (fun d -> if d.Length = 2 then param.Add(d.[0],System.Web.HttpUtility.UrlDecode(d.[1])))) 
    param
    
let parse_url (line:string) =
    let parts = line.Split(' ')
    if(parts.Length <2 || parts.Length>3) then failwith "400"
    let indexOfMark = parts.[1].IndexOf('?')
    
    if (indexOfMark>0) then
        let raw_query = parts.[1].Substring(indexOfMark+1)
        (parts.[0],parts.[1].Substring(0,indexOfMark),parse_data raw_query, "?" + raw_query)
    else 
        (parts.[0],parts.[1],empty_query_string (), String.Empty)
    
let read_post_data (stream:Stream) (bytes : int) = 
    stream.AsyncRead(bytes)
    
let parse_cookie (s:string) =
    s.Split(';') 
    |> Array.map (fun (x:string) -> 
                        let parts = x.Split('='); 
                        (parts.[0],parts.[1]))
                        
let parse_key_value_pairs arr =
    let dict = new Dictionary<string,string>()
    arr 
    |> Array.iter (fun (x:String) -> 
                    let parts = x.Split('=');
                    dict.Add(parts.[0],parts.[1]))
    dict
    
let header_params (header:string option) =
    match header with
    |Some(x) -> let parts = x.Split(';')|> Array.map (fun x -> x.TrimStart())
                parse_key_value_pairs (Array.sub parts 1 (parts.Length-1))
                
    |_ -> failwith "did not found header: %s." header
        
    
let parse_multipart (stream:Stream) boundary (request:HttpRequest) = 
    let rec loop boundary = 
        async {
        
        let! firstline = read_line stream
                
        if not(firstline.Equals("--")) then 
            
            let! part_headers = read_headers stream 
            let content_disposition = look_up part_headers "content-disposition"
            
            let fieldname = 
                (header_params content_disposition) ? name |> opt
                
            let content_type = look_up part_headers "content-type"
            
            match content_type with
            |Some(x) when x.StartsWith("multipart/mixed") 
                -> 
                    let subboundary = "--" + x.Substring(x.IndexOf('=')+1).TrimStart()
                    do! loop subboundary  
            |Some(x) 
                -> 
                    let temp_file_name = Path.GetTempFileName()
                    use temp_file = new FileStream(temp_file_name,FileMode.Truncate)
                    do! stream 
                        |> read_until (bytes("\r\n" + boundary)) (fun x y -> async { do! temp_file.AsyncWrite(x,0,y) }  )
                    let file_leght = temp_file.Length
                    temp_file.Close()
                    if  file_leght > int64(0) then
                        let filename = 
                            (header_params content_disposition) ? filename |> opt
                        request.Files.Add(new HttpUpload(fieldname,filename,content_type |> opt,temp_file_name))
                    else 
                        File.Delete(temp_file_name)
            |None-> 
                    use mem = new MemoryStream()
                    do! stream 
                        |> read_until (bytes("\r\n" + boundary)) (fun x y -> async { do! mem.AsyncWrite(x,0,y) }  )
                    
                    let byts = mem.ToArray()
                    request.Form.Add(fieldname, toString(byts,0,byts.Length))
            
            do! loop  boundary
    } 
    loop boundary   
     
let process_request proxyMode (stream:Stream) remoteip = async {
    
    let request = new HttpRequest()
    request.Stream <- stream
    request.RemoteAddress <- remoteip
    
    let! first_line = read_line stream 
    
    let (meth,url,query, raw_query) as q = parse_url (first_line)
    request.Url <- url
    request.Method <- meth
    request.Query <- query
    request.RawQuery <- raw_query
    
    let! headers = read_headers stream 
    request.Headers <- headers
    
    //wont continue parsing if on proxyMode with the intention of forwarding the stream as it is
    if not proxyMode then
        request.Headers 
        |> Seq.filter (fun x -> x.Key.Equals("cookie"))
        |> Seq.iter (fun x -> 
                        let cookie = parse_cookie x.Value; 
                        request.Cookies.Add (fst(cookie.[0]),cookie))
    
        if meth.Equals("POST") then 
            let content_enconding = headers.["content-type"]
            let content_length = Convert.ToInt32(headers.["content-length"])
        
            if content_enconding.StartsWith("application/x-www-form-urlencoded") then
                let! rawdata = read_post_data stream content_length
                let form_data = parse_data (toString (rawdata, 0, rawdata.Length))
                request.Form <- form_data
                request.RawForm <- rawdata
            elif content_enconding.StartsWith("multipart/form-data") then
                let boundary = "--" + content_enconding.Substring(content_enconding.IndexOf('=')+1).TrimStart()
                do! parse_multipart stream boundary request
            
    return request
} 

let response statusCode message (content:byte[]) (request:HttpRequest) = async {

    let stream:Stream = request.Stream
    
    do! async_writeln stream (sprintf "%s %d %s" proto_version statusCode message)
    do! async_writeln stream (sprintf "Server: Suave/%s (http://suaveframework.com)" suave_version)
    do! async_writeln stream (sprintf "X-Got-Pot: No")
    do! async_writeln stream (sprintf "Date: %s" (DateTime.Now.ToUniversalTime().ToString("R")))
    
    for (x,y) in request.Response.Headers do
        if not (List.exists (fun y -> x.ToLower().Equals(y)) ["server";"date";"content-length"]) then
           do! async_writeln stream (sprintf "%s: %s" x y )
    
    if not(request.Response.Headers.Exists(new Predicate<_>(fun (x,_) -> x.ToLower().Equals("content-type")))) then
        do! async_writeln stream (sprintf "Content-Type: %s" "text/html")
    
    if content.Length > 0 then 
        do! async_writeln stream (sprintf "Content-Length: %d" (content.Length))
        
    do! async_writeln stream ""
    
    if content.Length > 0 then
        do! async_writebytes stream content
}


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

open System.Net.Sockets

let request_loop webpart proto (processor: Stream -> String -> Async<HttpRequest>) (client:TcpClient) = async {
    try
    
        let keep_alive = ref true
        use stream = 
            client.GetStream()
            |> load_stream proto
        
        let remote_endpoint = (client.Client.RemoteEndPoint :?> IPEndPoint)
        let ipaddr = remote_endpoint.Address.ToString()

        while !keep_alive do
            use! request = processor stream ipaddr 
            let p = webpart request
            match p with 
            |Some(x) -> do! x
            |None -> ()
            stream.Flush()
            
            keep_alive := 
                match request.Headers ? connection with 
                |Some(x) when x.ToLower().Equals("keep-alive") -> true
                |_ -> false
            
        client.Close()
    with | :? EndOfStreamException -> Log.log "client disconnected.\n"
         | ex -> Log.log "client disconnected.\n%A" ex
}
    
let parallelize input f = input |> Seq.map (f)  |> Async.Parallel

type HttpBinding = Protocols * string * int      
type WebPart = HttpRequest -> Async<unit> Option

open Suave.Tcp
   
let web_server bindings (webpart:WebPart) =
    bindings 
    |> Array.map (fun (proto,ip,port) -> tcp_ip_server (ip,port) (request_loop webpart proto (process_request false)))
    |> Async.Parallel
    |> Async.Ignore

open System.Net

let copy_response_headers (headers1:WebHeaderCollection) (headers2:List<string*string>) =
    for e in headers1 do
        headers2.Add(e,headers1.[e])

let send_web_response (data:HttpWebResponse) (p: HttpRequest)  = async {
    copy_response_headers data.Headers (p.Response.Headers)
    let bytes = data.GetResponseStream() |> read_fully
    do! response (int data.StatusCode) (data.StatusDescription) bytes p
    }

let forward ip port (p: HttpRequest) =
    
    let buildWebHeadersCollection (h : Dictionary<string,string>) = 
        let r = new WebHeaderCollection()
        for e in h do
            if not (WebHeaderCollection.IsRestricted(e.Key)) then
                r.Add(e.Key,e.Value)
        r
    let url = new UriBuilder("http",ip,port,p.Url,p.RawQuery)
    let q:HttpWebRequest = WebRequest.Create(url.Uri) :?> HttpWebRequest
    q.Method <- p.Method
    q.Headers <- buildWebHeadersCollection(p.Headers)
    q.Proxy <- null
    //copy restricted headers
    match p.Headers ? accept with Some(v) -> q.Accept <- v | None -> ()
    //match p.Headers ? connection with Some(v) -> q.Connection <- v | None -> ()
    match p.Headers ? date with Some(v) -> q.Date <- DateTime.Parse(v) | None -> ()
    match p.Headers ? expect with Some(v) -> q.Expect <- v | None -> ()
    match p.Headers ? host with Some(v) -> q.Host <- v | None -> ()
    match p.Headers ? range with Some(v) -> q.AddRange(Int64.Parse(v)) | None -> ()
    match p.Headers ? referer with Some(v) -> q.Referer <- v | None -> ()
    match look_up p.Headers "content-type" with Some(v) -> q.ContentType <- v | None -> ()
    match look_up p.Headers "content-length" with Some(v) -> q.ContentLength <- Int64.Parse(v) | None -> ()
    match look_up p.Headers "if-modified-since" with Some(v) -> q.IfModifiedSince <- DateTime.Parse(v) | None -> ()
    match look_up p.Headers "transfer-encoding" with Some(v) -> q.TransferEncoding <- v | None -> ()
    match look_up p.Headers "user-agent" with Some(v) -> q.UserAgent <- v | None -> ()
    q.Headers.Add("X-Forwarded-For",p.RemoteAddress)
    async {
        if p.Method = "POST" then 
            let content_length = Convert.ToInt32(p.Headers.["content-length"])
            let req = q.GetRequestStream()
            //push unparsed POST data downstream
            let! remaining_bytes = p.Stream.AsyncRead(content_length)
            do! req.AsyncWrite(remaining_bytes)
        try
            let! data = q.AsyncGetResponse() 
            do! send_web_response (data :?> HttpWebResponse) p
        with
            | :? WebException as ex -> do! send_web_response (ex.Response :?> HttpWebResponse) p
    } |> succeed

let proxy proxyResolver (r:HttpRequest) =  
    match proxyResolver r with
    |None -> never
    |Some(ip,port) -> forward ip port

let proxy_server bindings resolver =
     bindings 
    |> Array.map (fun (proto,ip,port) -> tcp_ip_server (ip,port) (request_loop (warbler(fun http -> proxy resolver http)) proto (process_request true)))
    |> Async.Parallel
    |> Async.Ignore


