module Suave.Tests.Perf

open System.Reflection
open System.Net.Http

open Suave.Utils
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Web

open Suave.Tests.TestUtilities
open Suave.Testing

open Fuchu

open PerfUtil

let version =
  match Assembly.GetExecutingAssembly().GetCustomAttributes( typeof<AssemblyVersionAttribute>, true ) with
  | [| attr |] ->
    attr :?> AssemblyVersionAttribute
    |> fun a -> a.Version
  | _ -> "0.0.0.1"

type SuavePerfHarness(name, suave_config) =
  interface ITestable with
    member x.Name = name
    member x.Init() = ()
    member x.Fini() = ()
  member x.Serve part = suave_config part

[<Tests>]
let perf_tests =
  let inline iterate times (testF : unit -> unit) =
    for i = 1 to times do testF ()

  let server_factory = SuavePerfHarness("default config", runWith defaultConfig)

  let getFormValue name =
    request (fun x -> OK (x.formData name |> Option.get))

  let longData = String.replicate 1815 "A"

  let perfTest' name testImpl = perfTest name testImpl 1

  testList "performance tests" [
    testPerfHistory "perf-GET" server_factory version [
      perfTest' "GET /" <| fun harness ->
        iterate 400 (fun _ -> harness.Serve (OK "a") |> req HttpMethod.GET "/" None |> ignore)
      ]

    testPerfHistory "perf-POST" server_factory version [
      perfTest' "POST / n' mirror" <| fun harness ->
        iterate 400 (fun _ ->
          use data = new FormUrlEncodedContent(dict ["long", longData])
          Assert.Equal("expecting form data to be returned",
                       longData,
                       harness.Serve (getFormValue "long") |> req HttpMethod.POST "/" (Some data)))
      ]
    ]