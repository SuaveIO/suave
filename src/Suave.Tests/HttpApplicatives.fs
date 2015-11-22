module Suave.Tests.HttpApplicatives

open System

open Suave
open Suave.Http
open Suave.Http.WebPart.Operators
open Suave.Http.Operators
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Http.ServerErrors
open Suave.Web

open Suave.Tests.TestUtilities
open Suave.Testing

open Fuchu

[<Tests>]
let applicativeTests cfg =
  let runWithConfig = runWith cfg
  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    string binding.socketBinding.ip,
    int binding.socketBinding.port

  testList "primitives: Host applicative" [
    testCase "when not matching on Host" <| fun _ ->
      let app = request (fun r -> OK r.host)

      let res = runWithConfig app |> req HttpMethod.GET "/" None
      Assert.Equal("should be what config says the IP is", ip, res)

    testCase "when matching on Host but is forwarded" <| fun _ ->
      let app =
        host ip >>= request (fun r -> OK r.host)
        <|> warbler (fun ctx -> INTERNAL_ERROR (sprintf "host: %s" ctx.request.clientHostTrustProxy))

      let res = runWithConfig app |> req HttpMethod.GET "/" None
      Assert.Equal("should be what the config says the IP is", ip, res)
    ]