module Suave.Tests.Http2

open Fuchu

open System
open System.Collections.Specialized
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Threading
open System.Threading.Tasks

open Suave
open Suave.Operators
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Http2
open Suave.Huffman
open Suave.Utils
open Suave.Tests.TestUtilities
open Suave.Testing

open HttpTwo

[<Tests>]
let frameHeaders (_ : SuaveConfig) =
  testList "frame headers" [
    testCase "encode/decode" 
    <| fun _ ->
       let originalHeader = { length = 500u; ``type`` = 2uy; flags = 1uy; streamIdentifier = 16777215u }
       let encoded = encodeFrameHeader originalHeader
       let header = parseFrameHeader encoded
       Assert.Equal("encode >> decode == id", originalHeader, header)
  ]

[<Tests>]
let huffmanTest (_ : SuaveConfig) =
  let buf1 = Array.zeroCreate<byte> 4096
  let buf2 = Array.zeroCreate<byte> 4096
  testList "Huffman" [
    testPropertyWithConfig fsCheckConfig "encode/decode" 
    <| fun (testText:string) ->

       let decoder = Decoding.decode buf1 4096
       let encoder = Encoding.encode buf2
       
       let arr = System.Text.ASCIIEncoding.UTF8.GetBytes testText
       let encoded = encoder (new MemoryStream(arr,false))
       let b = decoder (new MemoryStream(encoded,false)) encoded.Length

       Assert.Equal("failed", testText, System.Text.ASCIIEncoding.UTF8.GetString b)
  ]

[<Tests>]
let http2connect (cfg: SuaveConfig) =

  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    string binding.socketBinding.ip,
    int binding.socketBinding.port

  testList "test basic http2" [
    testCase "basic request/response" 
    <| fun _ ->
         let ctx = runWith cfg (Successful.OK "hello")
         withContext (fun _ ->
           // open http connection
           let uri = Uri(sprintf "http://%s:%i/websocket" ip port)
           let http2 = new Http2Client(uri)
           let headers = new NameValueCollection ()
           let data = Array.zeroCreate<byte> 0
           let response = http2.Send(uri, HttpMethod.Get,headers, data)
           let result = ref null
           let cont = response.ContinueWith(fun (t:Task<Http2Client.Http2Response>) -> result := t.Result)
           cont.Wait()
           let body = Text.Encoding.UTF8.GetString((!result).Body)
           Assert.Equal("got something",body,"hello")) ctx
  ]