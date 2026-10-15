module Suave.Tests.HttpApplicatives

open System
open System.IO

open Suave
open Suave.Operators
open Suave.Filters
open Suave.Successful
open Suave.ServerErrors

open Suave.Tests.TestUtilities
open Suave.Testing

open Expecto

[<Tests>]
let applicativeTests cfg =
  let runWithConfig = runWith cfg
  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    string binding.socketBinding.ip,
    int binding.socketBinding.port

  testList "primitives: Host applicative" [
    testCase "url with spaces: path" <| fun _ ->
      let res = runWithConfig (path "/get by" >=> OK "A") |> req HttpMethod.GET "/get by" None
      Expect.equal res "A" "Should return A"

    testCase "url with spaces: pathScan" <| fun _ ->
      let res = runWithConfig (pathScan "/foo/%s" OK) |> req HttpMethod.GET "/foo/get by" None
      Expect.equal res "get by" "Should return 'get buy'"

    testCase "when not matching on Host" <| fun _ ->
      let app = request (fun r -> OK r.host)

      let res = runWithConfig app |> req HttpMethod.GET "/" None
      Expect.equal res ip "Should be what config says the IP is"

    testCase "when matching on Host but is forwarded" <| fun _ ->
      let app =
        host ip >=> request (fun r -> OK r.host)
        <|> warbler (fun ctx -> INTERNAL_ERROR (sprintf "host: %s" ctx.request.clientHostTrustProxy))

      let res = runWithConfig app |> req HttpMethod.GET "/" None
      Expect.equal res ip "Should be what the config says the IP is"
    ]

[<Tests>]
let deepCompositionTests =
  // Suave is compiled without .tail calls; `choose` recursing through its options
  // must still not grow the stack.
  let run (webPart : WebPart) =
    webPart { HttpContext.empty with request = { HttpRequest.empty with rawPath = "/" } }
    |> Async.RunSynchronously

  testList "deep web part composition" [
    testCase "choose over many non-matching options does not overflow the stack" <| fun _ ->
      let options = List.replicate 100000 never @ [ OK "found" ]
      Expect.isSome (run (choose options)) "The last option answers"
  ]

[<Tests>]
let logFormatTests =
  let ctx =
    let headers = Collections.Generic.List<_>([ "x-forwarded-for", "203.0.113.9"; "x-real-ip", "203.0.113.9" ])
    { HttpContext.empty with
        request = { HttpRequest.empty with rawPath = "/a"; headers = headers }
        response = { HttpContext.empty.response with status = HTTP_200.status }
        userState = Collections.Generic.Dictionary<_,_>() }

  testList "logFormat" [
    testCase "uses the 24-hour clock and a +0000 offset" <| fun _ ->
      let before = DateTime.UtcNow
      let line = logFormat ctx
      let after = DateTime.UtcNow
      let m = Text.RegularExpressions.Regex.Match(line, @"\[\d{2}/[A-Z][a-z]{2}/\d{4}:(\d{2}):\d{2}:\d{2} \+0000\]")
      Expect.isTrue m.Success (sprintf "timestamp should be in common log format: %s" line)
      Expect.contains [ before.Hour; after.Hour ] (int m.Groups.[1].Value) "hour should be on the 24-hour clock"

    testCase "logs the connection address, not X-Forwarded-For" <| fun _ ->
      let line = logFormat ctx
      Expect.stringStarts line (string ctx.connection.ipAddr + " ") "should log the connection address"
      Expect.isFalse (line.Contains "203.0.113.9") "should not log a client-supplied address"

    testCase "logFormatStructured matches logFormat" <| fun _ ->
      let template, fields = logFormatStructured ctx
      Expect.stringContains template "{utcNow:dd/MMM/yyyy:HH:mm:ss '+0000'}" "template should use the 24-hour clock"
      Expect.equal fields.["clientIp"] (box ctx.connection.ipAddr) "should log the connection address"
  ]