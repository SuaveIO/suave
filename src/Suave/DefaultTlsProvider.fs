namespace Suave

open Suave.Sockets
open Suave.Sockets.Control
open Suave.Utils.AsyncExtensions
open System.Security.Cryptography.X509Certificates
open System.Net.Security

type DefaultTlsTransport(cn : Connection, ssl : SslStream) =
  interface ITransport with
    member this.read (buf : ByteSegment) =
      async {
        try
          let! a = ssl.ReadAsync(buf.Array,buf.Offset,buf.Count)
          return Choice1Of2(a)
        with ex ->
          return Choice2Of2 <| ConnectionError (ex.ToString())
        }

     member this.write(buf : ByteSegment) =
      async {
        try 
          do! ssl.WriteAsync(buf.Array,buf.Offset,buf.Count)
          return Choice1Of2()
        with ex ->
          return Choice2Of2 <| ConnectionError (ex.ToString())
        
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