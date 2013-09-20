module Suave.Tcp

open System
open System.Net
open System.Net.Sockets

open Log

let MAX_BACK_LOG = Int32.MaxValue

type TcpListener with
    member x.AsyncAcceptTcpClient() = 
        Async.FromBeginEnd(x.BeginAcceptTcpClient,x.EndAcceptTcpClient)  

type TcpWorker<'a> = TcpClient ->  Async<'a>

let close (d:TcpClient) = 
    d.GetStream().Close();
    d.Close()
      
let stop_tcp (server:TcpListener) =
    printf "stopping server .. "
    server.Stop()
    printf "stopped\n"

let tcp_ip_server (sourceip,sourceport) (serve_client:TcpWorker<unit>)  = 
        
    log "starting listener:%s:%d\n" sourceip sourceport

    let server = new TcpListener(IPAddress.Parse(sourceip),sourceport)
    server.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, (int)1)
    server.Start(MAX_BACK_LOG)
    //consider: 
    //echo 5 > /proc/sys/net/ipv4/tcp_fin_timeout
    //echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
    //custom kernel with shorter TCP_TIMEWAIT_LEN in include/net/tcp.h
    let job (d:#TcpClient) = async {
            use! oo = Async.OnCancel ( fun () -> log "disconnected client\n";close d)
            try
                try
                    do! serve_client d 
                with 
                    | :? System.IO.EndOfStreamException -> log "disconnected client\n"
                    | x -> log "Tcp request processing failed.\n%A\n" x
            finally close d
            }

    async {
        
        try
            use! dd = Async.OnCancel( fun () -> stop_tcp server)

            let! token = Async.CancellationToken

            while not(token.IsCancellationRequested) do
                let! client = server.AsyncAcceptTcpClient() 
                //let remoteAddress = (client.Client.RemoteEndPoint :?> IPEndPoint).Address
                Async.Start (job client, token)
                
        with x -> log "Tcp server failed.\n%A\n" x
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

