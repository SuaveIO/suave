module Suave.Tcp

open System.Net
open System.Net.Sockets

type TcpListener with
    member x.AsyncAcceptTcpClient() = 
        Async.FromBeginEnd(x.BeginAcceptTcpClient,x.EndAcceptTcpClient)  

type TcpWorker<'a> = TcpClient ->  Async<'a>
     
let tcp_ip_server (sourceip,sourceport) serve_client = async {

    let server = new TcpListener(IPAddress.Parse(sourceip),sourceport)
    server.Start()

    while true do
        
        let! client = server.AsyncAcceptTcpClient() 
        serve_client client |> Async.Start
}

let stream (client:TcpClient) = client.GetStream()

open System.IO

let mirror (clientStream:Stream) (serverStream:Stream) = async {
    while true do
        let! onebyte = clientStream.AsyncRead(1)
        do! serverStream.AsyncWrite(onebyte) 
}        

