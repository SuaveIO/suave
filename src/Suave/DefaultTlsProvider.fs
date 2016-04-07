namespace Suave

open Suave.Sockets
open Suave.Sockets.Control
open System.Security.Authentication
open System.Security.Cryptography.X509Certificates
open System.Net.Security

type DefaultTlsTransport(cn : Connection, ssl : SslStream) =
  interface ITransport with
    member this.read (buf : ByteSegment) =
      socket { return ssl.Read(buf.Array,buf.Offset,buf.Count) }
    member this.write(buf : ByteSegment) =
      socket { return ssl.Write(buf.Array,buf.Offset,buf.Count) }
    member this.shutdown() =
      cn.transport.shutdown()

type DefaultTlsProvider() = 
  interface TlsProvider with
    member this.wrap(connection : Connection, cert : obj) = socket {
      let sslStream = new SslStream(new TransportStream(connection.transport))
      #if NETSTANDARD1_5
      do! SocketOp.ofTask <| sslStream.AuthenticateAsServerAsync (cert :?> X509Certificate)
      #else
      sslStream.AuthenticateAsServer (cert :?> X509Certificate)
      #endif
      let tlsTransport = new DefaultTlsTransport(connection, sslStream)
      return { connection with transport = tlsTransport }
    }