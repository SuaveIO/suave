namespace Suave

open Suave.Sockets
open Suave.Sockets.Control

open System.Security.Authentication
open System.Security.Cryptography.X509Certificates;
open System.Net.Security;

type DefaultTlsTransport(cn : Connection, ssl : SslStream) =
  interface ITransport with
    member this.read (buf : ByteSegment) =
      socket { return ssl.Read(buf.Array,buf.Offset,buf.Count) }
    member this.write(buf : ByteSegment) =
      socket { return ssl.Write(buf.Array,buf.Offset,buf.Count) }
    member this.shutdown() =
      cn.transport.shutdown()

type DefaultTlsProvider() = 

  interface ITlsProvider with

    member this.Wrap(connection : Connection, cert : obj) = socket {
      let sslStream = new SslStream(new TransportStream(connection.transport))
      sslStream.AuthenticateAsServer (cert :?> X509Certificate)
      let tls_transport = new DefaultTlsTransport(connection, sslStream)
      return { connection with transport = tls_transport}
    }
