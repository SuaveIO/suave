module Suave.Tests

module Resp =
  open RestSharp

  let content (r : IRestResponse) = r.Content
  let content_encoding (r : IRestResponse) = r.ContentEncoding
  let content_length (r : IRestResponse) = r.ContentLength
  let content_type (r : IRestResponse) = r.ContentType
  
open System
open System.Threading
open System.Net.Http

open Suave.Types
open Suave.Web
open Suave.Http

open Fuchu
open RestSharp
open Resp

module RequestFactory =
  type SuaveTestCtx =
    { cts : CancellationTokenSource
    ; suave_config : Suave.Types.Config }

  let run_with config web_parts : SuaveTestCtx =
    let binding = config.bindings.Head
    let base_uri = binding.ToString()
    let cts = new CancellationTokenSource()
    let config' = { config with ct = cts.Token }

    let listening, server = web_server_async config web_parts
    Async.Start(server, cts.Token)
    listening |> Async.RunSynchronously // wait for the server to start listening

    { cts = cts
    ; suave_config = config' }

  let req methd (resource : string) extractor ctx =
    let client = RestClient(ctx.suave_config.bindings.Head.ToString())
    let request = RestRequest(resource, methd)
    let res = client.Execute request |> extractor
    Log.log "finished requesting %O" request
    try
      ctx.cts.Cancel()
      ctx.cts.Dispose()
    with e -> Log.log "exn: %O" e
    res

[<Tests>]
let smoking =
  testList "smoking hot" [ testCase "smoke" <| fun _ -> Assert.Equal("smoke test", true, true) ]

[<Tests>]
let utilities =
  testList "trying some utility functions" [
    testCase "loopback ipv4" <|
      fun _ -> Assert.Equal("127.0.0.1 is a local address", true, is_local_address "127.0.0.1")
    testCase "loopback ipv6" <|
      fun _ -> Assert.Equal("::0 is a local address", true, is_local_address "::1")
  ]

open RequestFactory

[<Tests>]
let gets =
  let run_with' = run_with default_config

  testList "getting basic responses"
    [
      testProperty "200 OK returns equivalent" <|
        fun resp_str -> run_with' (OK resp_str) |> req Method.GET "/hello" content = resp_str
    ]

[<EntryPoint>]
let main args =
  let r = defaultMainThisAssembly args
  System.Console.ReadKey true |> ignore
  r
