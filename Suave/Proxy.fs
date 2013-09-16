module Suave.Proxy

open System
open System.Net
open System.Collections.Generic

open Suave.Types
open Suave.Http
open Suave.Web
open Suave.Tcp

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

