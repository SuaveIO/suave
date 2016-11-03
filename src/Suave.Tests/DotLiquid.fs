module Suave.Tests.DotLiquid

open Expecto
open Suave
open System.IO

type M1 =
  { name : string }

[<Tests>]
let tests =
  testList "DotLiquid templating with Suave" [
    DotLiquid.setTemplatesDir TestUtilities.currentPath
    let combine lastBit = Path.Combine(TestUtilities.currentPath, lastBit)

    yield testCase "can render a page" <| fun () ->
      let subject =
        DotLiquid.renderPageFile (combine "liquid/hello.liquid") { M1.name = "haf" }
        |> Async.RunSynchronously
      Expect.equal subject "Hi haf" "should render properly"

    yield testCase "can render a page & master" <| fun () ->
      let subject =
        DotLiquid.renderPageFile (combine "liquid/child.liquid") { M1.name = "haf2" }
        |> Async.RunSynchronously
      Expect.equal subject "Parent: Hi haf2"  "should render parent and child"

    yield testCase "can render from string" <| fun () ->
      let subject =
        DotLiquid.renderPageString "Hi {{ model.name }}" { M1.name = "haf3" }
        |> Async.RunSynchronously

      Expect.equal subject "Hi haf3" "should render Hello"
    ]
