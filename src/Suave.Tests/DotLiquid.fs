module Suave.Tests.DotLiquid

open Fuchu
open Suave
open System
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
      Assert.Equal("should render properly", "Hi haf", subject)

    yield testCase "can render a page & master" <| fun () ->
      let subject =
        DotLiquid.renderPageFile (combine "liquid/child.liquid") { M1.name = "haf2" }
        |> Async.RunSynchronously
      Assert.Equal("should render parent and child", "Parent: Hi haf2" , subject)

    yield testCase "can render from string" <| fun () ->
      let subject =
        DotLiquid.renderPageString "Hi {{ model.name }}" { M1.name = "haf3" }
        |> Async.RunSynchronously

      Assert.Equal("should render Hello", "Hi haf3", subject)
    ]
