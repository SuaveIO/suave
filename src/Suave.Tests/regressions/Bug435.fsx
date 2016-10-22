#r "../bin/Debug/Suave.dll"

open System.Collections.Generic
open System.IO
open System.Net

open Suave
open Suave.Owin
open System.Threading
    
let hello = "Hello, OWIN!"B

let owinHelloWorld (env : OwinEnvironment) =    

    env.[OwinConstants.responseStatusCode] <- box 200

    // set content-length header
    let responseHeaders : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
    responseHeaders.["Content-Length"] <- [| string hello.Length |]

    let responseStream : Stream = unbox env.[OwinConstants.responseBody]
    responseStream.Write(hello, 0, hello.Length)
    async.Return ()

let app = OwinApp.ofApp "/" owinHelloWorld

let startup, server = startWebServerAsync defaultConfig app
let cts = new CancellationTokenSource()
Async.Start(server, cts.Token)

async {
    let! startData = startup
    let startData = startData |> Array.filter Option.isSome |> Array.map Option.get
    match startData with
    | [||] -> ()
    | _ ->
        let webClient = new WebClient()
        let uri = (defaultConfig.bindings.[0].uri "/" "")
        printfn "GET: %A" uri
        let data = webClient.DownloadString uri
        for header in webClient.ResponseHeaders.AllKeys do
            printfn "%s: %s" header (webClient.ResponseHeaders.[header])
        printfn "%s" data        
        cts.Cancel()
}
|> Async.RunSynchronously
