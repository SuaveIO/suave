namespace Suave

/// Parsing and control flow handling for web requests
module internal ParsingAndControl =

  open System.IO
  open Suave.Sockets.Control
  open Tcp

  /// The request loop initialises a request with a processor to handle the
  /// incoming stream and possibly pass the request to the web parts, a protocol,
  /// a web part, an error handler and a Connection to use for read-write
  /// communication -- getting the initial request stream.
  let inline requestLoop (consumer : WebPart) (facade : ConnectionFacade) =
    task {
      let flag = ref true
      while !flag do
        let! b = facade.loop consumer
        match b with
        | Ok b -> flag := b
        | _ -> flag := false
      return ()
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