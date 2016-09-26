module Suave.Tests.OwinContentLength

  open System.Collections.Generic
  open System.IO
  open System.Net

  open Suave
  open Suave.Owin
  open Suave.Testing

  open Fuchu
  
  let hello = "Hello, OWIN!"B
  let contentLengthHeader = "Content-Length"

  let owinHelloWorld (env : OwinEnvironment) =  

    env.[OwinConstants.responseStatusCode] <- box 200

    // set content-length header
    let responseHeaders : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
    responseHeaders.[contentLengthHeader] <- [| string hello.Length |]

    let responseStream : Stream = unbox env.[OwinConstants.responseBody]
    responseStream.Write(hello, 0, hello.Length)
    async.Return ()

  let app = OwinApp.ofApp "/" owinHelloWorld

  [<Tests>]
  let tests (cfg : SuaveConfig) =
    let runWithConfig = runWith cfg
    let uriFor (res : string) = SuaveConfig.firstBindingUri cfg res ""

    testCase "content-length header sent" <| fun _ ->
      let ctx = runWithConfig app
      try
        let uri = (uriFor "/")
        let webClient = new WebClient()
        webClient.DownloadString uri |> ignore
        Assert.Equal( "Content Length Header", (string hello.Length), webClient.ResponseHeaders.[contentLengthHeader])

      finally
        disposeContext ctx
      ()
