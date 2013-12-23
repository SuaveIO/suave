module Suave.Tests

module Resp =
  open System.Net.Http
  let content (r : HttpResponseMessage) = r.Content
  
open System
open System.Threading
open System.Net.Http

open Suave.Types
open Suave.Web
open Suave.Http

open Fuchu
open Resp

type Method =
  | GET
  | POST
  | DELETE
  | PUT
  | HEAD
  | CONNECT
  | PATCH
  | TRACE
  | OPTIONS

module RequestFactory =
  type SuaveTestCtx =
    { cts          : CancellationTokenSource
    ; suave_config : Suave.Types.Config }
    member x.Destroy() =
      x.cts.Cancel()
      x.cts.Dispose()

  let run_with config web_parts : SuaveTestCtx =
    let binding = config.bindings.Head
    let base_uri = binding.ToString()
    let cts = new CancellationTokenSource()
    cts.Token.Register(fun () -> Log.log "tests:run_with - cancelled") |> ignore
    let config' = { config with ct = cts.Token }

    let listening, server = web_server_async config web_parts
    Async.Start(server, cts.Token)
    Log.log "tests:run_with -> listening"
    listening |> Async.RunSynchronously // wait for the server to start listening
    Log.log "tests:run_with <- listening"

    { cts = cts
    ; suave_config = config' }

  let req (methd : Method) (resource : string) ctx =
    let server = ctx.suave_config.bindings.Head.ToString()
    use client = new System.Net.Http.HttpClient(BaseAddress = Uri server)
    Log.log "tests:req GET %s, resource %s -> execute" server resource
    let res = client.GetAsync(resource, HttpCompletionOption.ResponseContentRead, ctx.cts.Token).Result
    Log.log "tests:req GET %s, resource %s <- execute" server resource
    ctx.Destroy()
    res.Content.ReadAsStringAsync().Result

[<Tests>]
let smoking =
  testList "smoking hot" [
    testCase "smoke" <| fun _ -> Assert.Equal("smoke test", true, true)
  ]

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
      testCase "200 OK returns 'a'" <|
        fun _ -> Assert.Equal("expecting non-empty response", "a", run_with' (OK "a") |> req GET "/")

      testProperty "200 OK returns equivalent" <|
        fun resp_str -> (run_with' (OK resp_str) |> req GET "/hello") = resp_str

      testCase "204 No Content empty body" <|
        fun _ -> Assert.Equal("empty string should always be returned by 204 No Content",
                              "", (run_with' NO_CONTENT |> req GET "/"))
    ]

[<EntryPoint>]
let main args =
  let r = defaultMainThisAssembly args
  r
