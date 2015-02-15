module Suave.Tests.HttpApplicatives

open System

open Suave
open Suave.Types
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Http.ServerErrors
open Suave.Web

open Suave.Tests.TestUtilities
open Suave.Testing

open Fuchu

[<Tests>]
let tests =
  let runWithConfig = runWith defaultConfig

  testList "primitives: Host applicative" [
    testCase "when not matching on Host" <| fun _ ->
      let app =
        request (fun r ->
          match r.host with
          | ClientOnly h -> OK h
          | x -> INTERNAL_ERROR (sprintf "shouldn't match %A" x))
      let res = runWithConfig app |> req HttpMethod.GET "/" None
      Assert.Equal("should be IPv4 loopback", "127.0.0.1", res)

    testCase "when matching on Host" <| fun _ ->
      let app =
        host "127.0.0.1"
          >>= request (fun r ->
            match r.host with
            | ServerClient h -> OK h
            | x -> INTERNAL_ERROR (sprintf "shouldn't match %A" x))
      let res = runWithConfig app |> req HttpMethod.GET "/" None
      Assert.Equal("should be IPv4 loopback", "127.0.0.1", res)

    testCase "when matching on Host but is forwarded" <| fun _ ->
      Tests.skiptest "not done, see issue #103"
      let app =
        host "127.0.0.1"
          >>= request (fun r ->
          match r.host with
          | Forwarded (from, ServerClient h) -> OK h
          | x -> INTERNAL_ERROR (sprintf "shouldn't match %A" x))
      let res = runWithConfig app |> req HttpMethod.GET "/" None
      Assert.Equal("should be IPv4 loopback", "127.0.0.1", res)
    ]