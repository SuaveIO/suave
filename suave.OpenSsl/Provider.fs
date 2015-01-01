module Suave.OpenSSL.Provider

open OpenSSL.X509
open OpenSSL.Core

open Suave.Sockets
open Suave.Types

open Suave.OpenSSL.Functions

type TlsTransport(cn : Connection, ssl) =
  interface ITransport with
    member this.read (buf : ByteSegment) = ssl_receive cn ssl buf
    member this.write(buf : ByteSegment) = ssl_send cn  ssl buf

type OpenSSLProvider(cert : X509Certificate) = 

  interface ITlsProvider with

    member this.Wrap(connection : Connection) = socket {
      let ssl = authenticate_as_server cert
      do! accept connection ssl
      let tls_transport = new TlsTransport(connection, ssl)
      return { connection with transport = tls_transport}
    }

let open_ssl cert = new OpenSSLProvider(cert)