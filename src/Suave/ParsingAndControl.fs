namespace Suave

/// Parsing and control flow handling for web requests
module internal ParsingAndControl =

  open System.IO
  open Suave.Sockets.Control
  open Tcp

  /// Load a readable plain-text stream, based on the protocol in use. If plain HTTP
  /// is being used, the stream is returned as it, otherwise a new SslStream is created
  /// to decipher the stream, without client certificates.
  let inline loadConnection (facade : ConnectionFacade) = socket{
    match facade.Runtime.matchedBinding.scheme with
    | HTTP    ->
      return ()
    | HTTPS o ->
      let! tlsConnection = facade.Runtime.tlsProvider.wrap (facade.Connection,o)
      facade.Connection <- tlsConnection
    }

  /// The request loop initialises a request with a processor to handle the
  /// incoming stream and possibly pass the request to the web parts, a protocol,
  /// a web part, an error handler and a Connection to use for read-write
  /// communication -- getting the initial request stream.
  let inline requestLoop (consumer   : WebPart) (facade : ConnectionFacade) =
    async {
      let! a = loadConnection facade
      do! facade.loop consumer
    }

  /// Starts a new web worker, given the configuration and a web part to serve.
  let startWebWorkerAsync (webpart : WebPart) (runtime : HttpRuntime) runServer =
    startTcpIpServerAsync (requestLoop webpart)
                          runtime.matchedBinding.socketBinding
                          runServer

  let resolveDirectory homeDirectory =
    match homeDirectory with
    | None   -> Path.GetDirectoryName(System.AppContext.BaseDirectory)
    | Some s -> s