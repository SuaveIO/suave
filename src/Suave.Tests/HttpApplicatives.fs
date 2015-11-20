module Suave.Tests.HttpApplicatives

open System

open Suave
open Suave.Http
open Suave.Http.Operators
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Http.ServerErrors
open Suave.Web

open Suave.Tests.TestUtilities
open Suave.Testing

open Fuchu

[<Tests>]
let tests cfg =
  let runWithConfig = runWith cfg

  testList "primitives: Host applicative" [
    testCase "when not matching on Host" <| fun _ ->
      let app = request (fun r -> OK r.host)

      let res = runWithConfig app |> req HttpMethod.GET "/" None
      Assert.Equal("should be IPv4 loopback", "127.0.0.1", res)

    testCase "when matching on Host but is forwarded" <| fun _ ->
      let app =
        host "127.0.0.1" >>= request (fun r -> OK r.host)
        <|> warbler (fun ctx -> INTERNAL_ERROR (sprintf "host: %s" ctx.request.clientHostTrustProxy))
      let res = runWithConfig app |> req HttpMethod.GET "/" None
      Assert.Equal("should be IPv4 loopback", "127.0.0.1", res)
    ]