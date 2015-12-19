module Suave.OpenSSL.Provider

open OpenSSL.X509
open OpenSSL.Core

open Suave.Sockets
open Suave.Sockets.Control
open Suave.Http

open Suave.OpenSSL.Functions

type TlsTransport(cn : Connection, ssl) =
  interface ITransport with
    member this.read (buf : ByteSegment) = sslReceive cn ssl buf
    member this.write(buf : ByteSegment) = sslSend cn  ssl buf
    member this.shutdown() =
      cn.transport.shutdown()

type OpenSSLProvider() = 

  interface TlsProvider with

    member this.wrap(connection : Connection, cert : obj) = socket {
      let ssl = authenticateAsServer (cert :?> X509Certificate)
      do! accept connection ssl
      let tls_transport = new TlsTransport(connection, ssl)
      return { connection with transport = tls_transport}
    }