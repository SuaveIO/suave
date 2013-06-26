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

let challenge  =
    set_header "WWW-Authenticate" "Basic realm=\"protected\"" 
    >> response 401 "Authorization Required" (bytes "401 Unauthorized.")

let ok s  = response  200 "OK" s  >> succeed 

let OK a = ok (bytes a);

let failure message = response 500 "Internal Error" message  >> succeed 

let ERROR a = failure (bytes a)

let redirect url  = 
    set_header "Location" url
    >> response 302 url (bytes "Content Moved")
    >> succeed 

let unhandled message = response 404 "Not Found" message  >>  succeed 

let notfound message = unhandled (bytes message)

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
        set_header "Content-Type" mimes  >> ok (File.ReadAllBytes(filename)) 
    else
        never

let local_file str = sprintf "%s%s" Environment.CurrentDirectory str        

let browse (req:HttpRequest) =  file (local_file req.Url) 

let dir url =   

    let dirname = local_file url
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
        ok (bytes (result.ToString())) 
    else never 

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
    let bytes = bytes (sprintf "%A\n" (http_request.Method, http_request.RemoteAddress, http_request.Url, http_request.Query, http_request.Form, http_request.Headers))
    s.Write(bytes,0,bytes.Length)
    succeed http_request

open Suave.Sscanf

let urlscan (pf:PrintfFormat<_,_,_,_,'t>) (h: 't ->  WebPart) :  WebPart = 
        
    let t url = sscanf pf url 
        
    let F (r:HttpRequest) = 
        try
            let y = r.Url |> t |> h
            try y r with ex -> r |> ERROR (ex.ToString())
        with _ -> fail
    F