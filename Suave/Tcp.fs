module Suave.Tcp

open System
open System.Net
open System.Net.Sockets

open Log
let log_str = log "%s"

/// The max backlog of number of requests
let MAX_BACK_LOG = Int32.MaxValue

/// Asynchronous extension methods to TcpListener to make
/// it nicer to consume in F#
type TcpListener with
  member x.AsyncAcceptTcpClient() =
    Async.FromBeginEnd(x.BeginAcceptTcpClient,x.EndAcceptTcpClient)

/// A TCP Worker is a thing that takes a TCP client and returns an asynchronous workflow thereof
type TcpWorker<'a> = TcpClient ->  Async<'a>

/// Close the TCP client by closing its stream and then closing the client itself
let close (d : TcpClient) =
    if d.Connected then
        d.GetStream().Close()
        d.Close()

/// Stop the TCP listener server
let stop_tcp (server : TcpListener) =
  log_str "stopping server .. "
  server.Stop()
  log_str "stopped\n"

/// Start a new TCP server with a specific IP, Port and with a serve_client worker
let tcp_ip_server (sourceip, sourceport) (serve_client : TcpWorker<unit>) =

  log "starting listener:%s:%d\n" sourceip sourceport

  let server = new TcpListener(IPAddress.Parse(sourceip), sourceport)
  server.Server.SetSocketOption(SocketOptionLevel.Socket, SocketOptionName.ReuseAddress, (int)1)
  server.Start MAX_BACK_LOG
  //consider:
  //echo 5 > /proc/sys/net/ipv4/tcp_fin_timeout
  //echo 1 > /proc/sys/net/ipv4/tcp_tw_recycle
  //custom kernel with shorter TCP_TIMEWAIT_LEN in include/net/tcp.h
  let job (d:#TcpClient) = async {
    use! oo = Async.OnCancel ( fun () -> log "disconnected client\n"; close d)
    try
      try
        do! serve_client d
      with 
        | :? System.IO.EndOfStreamException -> log "disconnected client\n"
        | x -> log "Tcp request processing failed.\n%A\n" x
    finally close d
  }

  // start a new async worker for each accepted TCP client
  async {
    try
      use! dd = Async.OnCancel(fun () -> stop_tcp server)
      let! (token : Threading.CancellationToken) = Async.CancellationToken
      while not (token.IsCancellationRequested) do
        let! client = server.AsyncAcceptTcpClient()
        //let remoteAddress = (client.Client.RemoteEndPoint :?> IPEndPoint).Address
        Async.Start (job client, token)
    with x -> log "Tcp server failed.\n%A\n" x
  }

/// Get the stream from the TCP client
let stream (client : TcpClient) = client.GetStream()

open System.IO

/// Mirror the stream byte-by-byte, one byte at a time
let mirror (clientStream:Stream) (serverStream:Stream) = async {
  try
  while true do
    let! onebyte = clientStream.AsyncRead(1)
    do! serverStream.AsyncWrite(onebyte)
  with _ -> ()
}
