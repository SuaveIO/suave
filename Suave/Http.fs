module Suave.Http

open Web
open System
open System.IO
open System.Text

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

(*note: we may refactor the function content down here and just pass the content bytes*)
let response statusCode message (content:HttpRequest -> byte[]) (http_request:HttpRequest) = async {

    do! async_writeln (http_request.Stream) (sprintf "%s %d %s" proto_version statusCode message)
    do! async_writeln (http_request.Stream) (sprintf "Server: Suave/%s (http://suaveframework.com)" suave_version)
    do! async_writeln (http_request.Stream) (sprintf "X-Got-Pot: No")
    do! async_writeln (http_request.Stream) (sprintf "Date: %s" (DateTime.Now.ToUniversalTime().ToString("R")))
    
    for (x,y) in http_request.Response.Headers do
        do! async_writeln (http_request.Stream) (sprintf "%s: %s" x y )
    
    if not(http_request.Response.Headers.Exists(new Predicate<_>(fun (x,_) -> x.ToLower().Equals("content-type")))) then
        do! async_writeln (http_request.Stream) (sprintf "Content-Type: %s" "text/html")
    
    let content_bytes = content http_request
    
    if content_bytes.Length > 0 then 
        do! async_writeln (http_request.Stream) (sprintf "Content-Length: %d" (content_bytes.Length))
        
    do! async_writeln (http_request.Stream) ""
    
    if content_bytes.Length > 0 then
        do! async_writebytes (http_request.Stream) content_bytes
}

let challenge  =
    set_header "WWW-Authenticate" "Basic realm=\"protected\"" 
    >> response 401 "Authorization Required" (bytes "401 Unauthorized." |> cnst)

let ok s  = response  200 "OK" s >> succeed 

let OK a = ok (bytes a |> cnst);

let failure message = response 500 "Internal Error" message >> succeed 

let ERROR a = failure (bytes a |> cnst)

let redirect url  = 
    set_header "Location" url
    >> response 302 url (bytes "Content Moved" |> cnst)
    >> succeed 

let unhandled message = response 404 "Not Found" message >>  succeed 

let notfound message = unhandled (cnst (bytes message))

let mime_type = function
    |".bmp" -> "image/bmp"
    |".css" -> "text/css"
    |".gif" -> "image/gif"
    |".png" -> "image/png"
    |".ico" -> "image/x-icon"
    |".htm" 
    |".html" -> "text/html";
    |".jpe" 
    |".jpeg"
    |".jpg" -> "image/jpeg"
    |".js" -> "application/x-javascript"
    |".exe" -> "application/exe"
    |_ -> "application/octet-stream"

let file filename  = 
    if File.Exists(filename) then
        let file_info = new FileInfo(filename)
        let mimes = mime_type (file_info.Extension)
        //TODO: file should be read async
        set_header "Content-Type" mimes  >> ok (File.ReadAllBytes(filename) |> cnst) 
    else
        never

let local_file str = sprintf "%s%s" Environment.CurrentDirectory str        

let browse (http_request:HttpRequest) = 
    file (local_file http_request.Url) http_request
    
let dir (http_request:HttpRequest) =   

    let dirname = local_file http_request.Url
    let result = new StringBuilder()

    let filesize  (x:FileSystemInfo) =  
        if (x.Attributes ||| FileAttributes.Directory =  FileAttributes.Directory) then 
            String.Format("{0,-14}","<DIR>")
        else 
            String.Format("{0,14}",(new FileInfo(x.FullName)).Length)
            
    let formatdate (t:DateTime) = 
        t.ToString("MM-dd-yy") + "  " + t.ToString("hh:mmtt")

    let buildLine (x:FileSystemInfo) =  result.Append(x.LastWriteTime.ToString() + "       " + filesize(x) + " " + x.Name + "<br/>\n") |> ignore     
    
    if Directory.Exists(dirname) then
        let di = new DirectoryInfo(dirname)
        (di.GetFileSystemInfos()) |> Array.sortBy (fun x -> x.Name) |> Array.iter (buildLine)
        ok (bytes (result.ToString()) |> cnst) http_request
    else never http_request

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

open Suave.Sscanf

let urlscan (pf:PrintfFormat<_,_,_,_,'t>) (h: 't ->  WebPart) :  WebPart = 
    try
        let t url = sscanf pf url 
        let F (r:HttpRequest) = 
            let y = r.Url |> t |> h
            try
                y r
            with _ -> fail
        F
    with _ -> never