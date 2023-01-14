module Suave.Tests.Stream

open Expecto

open Suave
open Suave.Operators
open Suave.Testing
open System.IO
open System.Text

let private repeat n s =
  Seq.init n (fun _ -> s)
  |> String.concat ""

[<Tests>]
let streamTests (cfg : SuaveConfig) =
  let expected = repeat 16384 "Hello, world.\n"

  let makeStream =
    async {
      let stream = new MemoryStream(Encoding.UTF8.GetBytes expected)

      return stream :> Stream
    }

  let webPart =
    choose
      [
        Filters.path "/stream" >=> Stream.okStream makeStream
        Filters.path "/chunked" >=> Stream.okStreamChunked makeStream
      ]

  testList "stream tests" [
    testCase "okStream test" <| fun _ ->
      let ctx = runWith cfg webPart
      let res = req HttpMethod.GET "/stream" None ctx
      Expect.equal res expected "should return the expected content"

    testCase "okStreamChunked test" <| fun _ ->
      let ctx = runWith cfg webPart
      let res = req HttpMethod.GET "/chunked" None ctx
      Expect.equal res expected "should return the expected content"
  ]
