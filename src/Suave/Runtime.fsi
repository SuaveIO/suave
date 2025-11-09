namespace Suave

[<AutoOpen>]
module Runtime =
  open System
  open System.Net

  /// A port is an unsigned short (uint16) structure
  type Port = uint16

  type SocketBinding =
    { ip   : IPAddress
      port : Port }

    member endpoint : IPEndPoint

  module SocketBinding =
    val create : IPAddress -> Port -> SocketBinding

    /// A file's mime type and if compression is enabled or not
  type MimeType =
    { name        : string
      compression : bool }

  type MimeTypesMap = string -> MimeType option

    /// Gets the supported protocols, HTTP and HTTPS with a certificate
  type Protocol =
    /// The HTTP protocol is the core protocol
    | HTTP
    /// The HTTP protocol tunneled in a TLS tunnel
    | HTTPS of System.Security.Cryptography.X509Certificates.X509Certificate

    member secure : bool

  /// Type alias for string. This is the host as seen from the server; not
  /// necessarily as seen from the client.
  type Host = string

  /// A HTTP binding is a protocol is the product of HTTP or HTTP, a DNS or IP
  /// binding and a port number.
  type HttpBinding =
    { scheme        : Protocol
      socketBinding : SocketBinding }

    member uri : path:string -> query:string -> Uri

    /// Overrides the default ToString() method to provide an implementation that
    /// is assignable to a BaseUri for a RestClient/HttpClient.
    override ToString : unit -> string

  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module HttpBinding =

    val DefaultBindingPort : Port

    /// This is the value of the default HttpBinding.
    val defaults : HttpBinding

    /// Create a HttpBinding for the given protocol, an IP address to bind to and
    /// a port to listen on – this is the strongly typed overload.
    val create : scheme:Protocol -> ip:IPAddress -> port:Port -> HttpBinding

    /// Create a HttpBinding for the given protocol, an IP address to bind to and
    /// a port to listen on – this is the "stringly typed" overload.
    val createSimple : scheme:Protocol -> ip:string -> port:int -> HttpBinding

  /// A server-key is a 256 bit key with high entropy
  type ServerKey = byte []

  /// Utilities to ensure server keys are well-formed
  [<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
  module ServerKey =
    
    /// Ensure that a server key is the proper length
    val validate : ServerKey -> ServerKey

    /// Create a key from a base-64 encoded string
    val fromBase64 : (string -> ServerKey)

  type IPAddress with
    /// Try parse the IP address from a string, returning a choice.
    static member tryParseC : ip:string -> Choice<IPAddress, unit>

  /// A holder for uploaded file meta-data
  type HttpUpload =
    { fieldName    : string
      fileName     : string
      mimeType     : string
      tempFilePath : string }
