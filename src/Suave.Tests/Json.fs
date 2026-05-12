module Suave.Tests.Json

open Expecto

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Suave
open Suave.Json

open Suave.Tests.TestUtilities
open Suave.Testing

open System.Runtime.Serialization

[<DataContract>]
type Foo =
  { 
  [<field: DataMember(Name = "foo")>]
  foo : string;
  }

[<DataContract>]
type Bar =
  { 
  [<field: DataMember(Name = "bar")>]
  bar : string;
  }

[<Tests>]
let parsingMultipart cfg =
  let runWithConfig = runWith cfg

  testList "mapJson test" [
    testCase "simple test" <| fun _ ->
      Assert.Equal("returns correct json representation", "{\"bar\":\"foo\"}", 
        runWithConfig (mapJson (fun (a:Foo) -> { bar = a.foo })) |> req HttpMethod.POST "/" (Some <| new ByteArrayContent(toJson { foo = "foo" }))) ]
