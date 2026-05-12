module Suave.Tests.Connection

open System
open System.Net
open System.Net.Http

open Expecto

open Suave
open Suave.Successful

open Suave.Testing

let setConnectionKeepAlive (r : HttpRequestMessage) =
  r.Headers.ConnectionClose <- Nullable false
  r

// several times the size of a buffer
let veryLargeContent = String.replicate (defaultConfig.bufferSize * 1000) "A"

[<Tests>]
let connectionTests cfg =
  testList "connecting" [
    testCase "connect with keep-alive default" <| fun _ ->
      let context = runWith cfg (OK "ACK")
      let res =
        reqResp HttpMethod.GET "/"  "" None
                 None DecompressionMethods.None
                 setConnectionKeepAlive
                 contentString
                 context
      Expect.equal res "ACK" "should ACK"

    testCase "test large response" <| fun _ ->
      let context = runWith cfg (OK veryLargeContent)
      let res =
        req HttpMethod.GET "/" None context
      Expect.equal res.Length veryLargeContent.Length "should match"
    ]
