module Suave.Types

open System
open System.IO
open System.Collections.Generic

type HttpResponse()=
    let mutable headers: List<string*string> = new List<string*string>()
    member h.Headers with get() = headers and set x = headers <- x
    
type HttpUpload(fieldname:string,filename:string,mime_type:string,temp_file_name:string) =
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

open System.Security.Cryptography.X509Certificates;

type Protocols = | HTTP | HTTPS of X509Certificate
type HttpBinding = Protocols * string * int      
type WebPart = HttpRequest -> Async<unit> Option

type ErrorHandler = Exception -> String -> HttpRequest -> Async<unit>

type Config = { 
    bindings : HttpBinding array; 
    error_handler : ErrorHandler;
    timeout : int
    }

exception InternalFailure of string