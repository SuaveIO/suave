module Suave.Tests.Connection

open System
open System.Net
open System.Net.Http

open Fuchu

open Suave
open Suave.Successful

open Suave.Testing

let setConnectionKeepAlive (r : HttpRequestMessage) =
  r.Headers.ConnectionClose <- Nullable false
  r

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
      Assert.Equal("should ACK", "ACK", res)
    ]