
module Suave.OpenSsl.Provider

open OpenSSL.X509
open OpenSSL.Core

open Suave.Socket
open Suave.Types

open Suave.OpenSSL.Functions

type OpenSslProvider(cert : X509Certificate) = 

  interface ITlsProvider with

    member this.Wrap(connection : Connection) = socket {
      let ssl = authenticate_as_server cert
      do! accept connection ssl
      return { connection with read = ssl_receive connection ssl; write = ssl_send connection ssl }
    }

let open_ssl cert = new OpenSslProvider(cert)