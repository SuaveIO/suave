module Suave.Http

open System
open System.IO
open System.Text

open Utils
open Types

let set_header a b (http_request:HttpRequest) = 
    http_request.Response.Headers.Add(a,b)
    http_request

let set_cookie cookie = set_header  "Set-Cookie" cookie 
 
let url s (x:HttpRequest) = if s = x.Url then Some(x) else None
let meth0d s (x:HttpRequest) = if s = x.Method then Some(x) else None

let GET  (x:HttpRequest)  = meth0d "GET" x
let POST (x:HttpRequest)  = meth0d "POST" x
let DELETE (x:HttpRequest) = meth0d "DELETE" x
let PUT (x:HttpRequest) = meth0d "PUT" x

let suave_version = "0.1"
let proto_version = "HTTP/1.1"

let response statusCode message (content:byte[]) (request:HttpRequest) = async {
    
    try
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
    with //the connection might drop while we are sending the response
        | :? IOException as ex  -> raise (InternalFailure "Failure while writing to client stream")
}

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
    
let set_mime_type t = set_header "Content-Type" t

let file filename  = 
    if File.Exists(filename) then
        let file_info = new FileInfo(filename)
        let mimes = mime_type (file_info.Extension)
        //TODO: file should be read async
        set_mime_type mimes  >> ok (File.ReadAllBytes(filename)) 
    else
        never

let local_file str = sprintf "%s%s" Environment.CurrentDirectory str        

let browse : WebPart = warbler (fun req ->  file (local_file req.Url))

let browse_file filename = file (local_file filename)

type WebResult = Option<Async<unit>>

let dir (req: HttpRequest) : WebResult =  

    let url = req.Url

    let dirname = local_file url
    let result = new StringBuilder()

    let filesize  (x:FileSystemInfo) =  
        if (x.Attributes ||| FileAttributes.Directory =  FileAttributes.Directory) then 
            String.Format("{0,-14}",System.Web.HttpUtility.HtmlEncode("<DIR>"))
        else 
            String.Format("{0,14}",(new FileInfo(x.FullName)).Length)
            
    let formatdate (t:DateTime) = 
        t.ToString("MM-dd-yy") + "  " + t.ToString("hh:mmtt")

    let buildLine (x:FileSystemInfo) =  result.Append(x.LastWriteTime.ToString() + "       " + filesize(x) + " " + x.Name + "<br/>\n") |> ignore     
    
    if Directory.Exists(dirname) then
        let di = new DirectoryInfo(dirname)
        (di.GetFileSystemInfos()) |> Array.sortBy (fun x -> x.Name) |> Array.iter (buildLine)
        ok (bytes (result.ToString())) req
    else fail 

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
