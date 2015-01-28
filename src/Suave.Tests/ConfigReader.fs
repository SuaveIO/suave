module Suave.Tests.ConfigReader

open Fuchu
open System.IO
open Suave
open Suave.Types
open Suave.Tests.TestUtilities

[<Tests>]
let tests =
  let json = serialize default_config.props

  printfn "json str: %s" json

  File.WriteAllLines("test.json", [json])

  let path = Path.Combine(current_path, "test.json")
  let config = load_config path

  testList "ConfigReader" [
    testCase "test 1" <| fun _ ->
      match config with
      | Choice1Of2 a -> Assert.Equal("Config read from file does not match default_config.props", default_config.props, a)
      | Choice2Of2 b -> Assert.Equal("Path or file reading error", true, false)
    ]

  