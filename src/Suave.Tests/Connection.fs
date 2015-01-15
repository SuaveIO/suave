module Suave.Tests.Connection

open System
open System.Net
open System.Net.Http

open Fuchu

open Suave
open Suave.Web
open Suave.Types
open Suave.Http.Successful

open Suave.Testing

let set_connection_keep_alive (r : HttpRequestMessage) =
  r.Headers.ConnectionClose <- Nullable(false)
  r

[<Tests>]
let tests =
  testList "connecting" [
    testCase "connect with keep-alive default" <| fun _ ->
      let context = run_with default_config (OK "ACK")
      let res =
        req_resp HttpMethod.GET "/"  "" None
                 None DecompressionMethods.None
                 set_connection_keep_alive
                 content_string
                 context
      Assert.Equal("should ACK", "ACK", res)
    ]