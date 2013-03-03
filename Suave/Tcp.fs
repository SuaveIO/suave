module Suave.Tcp

open System
open System.Net
open System.Net.Sockets

open Log

type TcpListener with
    member x.AsyncAcceptTcpClient() = 
        Async.FromBeginEnd(x.BeginAcceptTcpClient,x.EndAcceptTcpClient)  

type TcpWorker<'a> = TcpClient ->  Async<'a>

let close (d:TcpClient) =
    d.Client.Shutdown(SocketShutdown.Both)
    d.Close()
    
let tcp_ip_server (sourceip,sourceport) (serve_client:TcpWorker<unit>)  = 
        
    log "starting listener:%s:%d\n" sourceip sourceport

    let server = new TcpListener(IPAddress.Parse(sourceip),sourceport)
    server.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, (int)1)
    server.Start()

    let job (d:#TcpClient) =  async {
            
            use! oo = Async.OnCancel ( fun () -> log "disconnected client\n";close d)
            try
            do! serve_client d
            with 
                | :? System.IO.EndOfStreamException -> close d
                | x -> 
                      log "%A\n" x
                      close d
            
            }

    async {
        
        try
        use! dd = Async.OnCancel( fun () -> printf "stopping server .. "; server.Stop();printf "stopped\n")

        let! token = Async.CancellationToken

        while not(token.IsCancellationRequested) do
            let! client = server.AsyncAcceptTcpClient() 
            let remoteAddress = (client.Client.RemoteEndPoint :?> IPEndPoint).Address
            Async.Start ((job client),token)
        with x -> log "%A" x
    }

let stream (client:TcpClient) = client.GetStream()

open System.IO

let mirror (clientStream:Stream) (serverStream:Stream) = async {

    try
    while true do
        let! onebyte = clientStream.AsyncRead(1)
        do! serverStream.AsyncWrite(onebyte) 
    with _ -> ()
}        

