module Suave.Tests.Perf

open Suave.Utils
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Tests.TestUtilities

open Fuchu

open System.Reflection
open System.Net.Http

open PerfUtil

let version =
  Assembly
    .GetExecutingAssembly()
    .GetCustomAttributes( typeof<AssemblyVersionAttribute>, true )
    |> Seq.head
    :?> AssemblyVersionAttribute
    |> fun a -> a.Version

type SuavePerfHarness(name, suave_config) =
  interface ITestable with
    member x.Name = name
  member x.Serve part = suave_config part

[<Tests>]
let perf_tests =
  let server_factory = SuavePerfHarness("default config", run_with default_config)

  let getFormValue name =
    request (fun x -> OK (x.form.[name]))

  let longData = String.replicate 1815 "A"

  testList "performance tests" [
    testPerfHistory "perf-GET" server_factory version [
      perfTest "GET /" <| fun harness ->
        repeat 400 (fun _ -> harness.Serve (OK "a") |> req GET "/" None |> ignore) ()
      ]

    testPerfHistory "perf-POST" server_factory version [
      perfTest "POST / n' mirror" <| fun harness ->
        repeat 400 (fun _ ->
          use data = new FormUrlEncodedContent(dict [ "long", longData])
          Assert.Equal("expecting form data to be returned", longData, harness.Serve (getFormValue "long") |> req POST "/" (Some data))) ()
      ]
    ]