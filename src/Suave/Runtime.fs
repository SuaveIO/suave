namespace Suave

open System
open Suave.Utils
open System.Net
open System.Net.Sockets

module Runtime =

  /// A port is an unsigned short (uint16) structure
  type Port = uint16

  type SocketBinding =
    { ip   : IPAddress
      port : Port }

    member x.endpoint =
      new IPEndPoint(x.ip, int x.port)

    override x.ToString() =
      if x.ip.AddressFamily = AddressFamily.InterNetworkV6 then
        String.Concat [ "["; x.ip.ToString(); "]:"; x.port.ToString() ]
      else
        String.Concat [ x.ip.ToString(); ":"; x.port.ToString() ]

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module SocketBinding =

    let create ip port =
      { ip = ip; port = port }

  type MimeType =
    { name        : string
      compression : bool }

  type MimeTypesMap = string -> MimeType option

  type Protocol =
    | HTTP
    | HTTPS of obj

    member x.secure =
      match x with
      | HTTP    -> false
      | HTTPS _ -> true

    override x.ToString() =
      match x with
      | HTTP    -> "http"
      | HTTPS _ -> "https"

  type Host = string

  type HttpBinding =
    { scheme        : Protocol
      socketBinding : SocketBinding }

    member x.uri (path : string) query =
      let path' =
        match Uri.TryCreate(path, UriKind.Absolute) with
        | true, uri when uri.Scheme = "http" || uri.Scheme = "https" -> uri.AbsolutePath
        | _ when path.StartsWith "/" -> path
        | _ -> "/" + path
      String.Concat [
        x.scheme.ToString(); "://"; x.socketBinding.ToString()
        path'
        (match query with | "" -> "" | qs -> "?" + qs)
      ]
      |> Uri

    override x.ToString() =
      String.Concat [ x.scheme.ToString(); "://"; x.socketBinding.ToString() ]

  type ServerKey = byte []

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module ServerKey =

    let validate (key : ServerKey) =
      if key.Length <> int Crypto.KeyLength then
        failwithf "Invalid server key length - should be %i, but was %i" Crypto.KeyLength key.Length
      key

    let fromBase64 =
      Convert.FromBase64String >> validate

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpBinding =

    let DefaultBindingPort = 8080us

    let defaults =
      { scheme        = HTTP
        socketBinding = SocketBinding.create IPAddress.Loopback DefaultBindingPort }

    let create scheme (ip : IPAddress) (port : Port) =
      { scheme        = scheme
        socketBinding = SocketBinding.create ip port }

    let createSimple scheme (ip: string) (port : int) =
      { scheme        = scheme
        socketBinding = SocketBinding.create (IPAddress.Parse ip) (uint16 port) }

  type IPAddress with
    static member tryParseC(ip: string) =
      match IPAddress.TryParse ip with
      | false, _ -> Choice2Of2 ()
      | _, ip    -> Choice1Of2 ip

  type HttpUpload =
    { fieldName    : string
      fileName     : string
      mimeType     : string
      tempFilePath : string }