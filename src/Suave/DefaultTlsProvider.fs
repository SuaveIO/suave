namespace Suave

open Suave.Sockets
open Suave.Sockets.Control
open Suave.Utils.AsyncExtensions
open System.Security.Cryptography.X509Certificates
open System.Net.Security

type DefaultTlsTransport(cn : Connection, ssl : SslStream) =
  interface ITransport with
    member this.read (buf : ByteSegment) =
      task {
        try
          let! a = ssl.ReadAsync(buf)
          return Ok(a)
        with ex ->
          return Result.Error <| ConnectionError (ex.ToString())
        }

     member this.write(buf : ByteSegment) =
      task {
        try 
          do! ssl.WriteAsync(buf)
          return Ok()
        with ex ->
          return Result.Error <| ConnectionError (ex.ToString())
        
        }
    member this.shutdown() =
      cn.transport.shutdown()

type DefaultTlsProvider() = 
  interface TlsProvider with
    member this.wrap(connection : Connection, cert : obj) = socket {
      let sslStream = new SslStream(new TransportStream(connection.transport))
      sslStream.AuthenticateAsServer (cert :?> X509Certificate)
      let tlsTransport = new DefaultTlsTransport(connection, sslStream)
      return { connection with transport = tlsTransport }
    }