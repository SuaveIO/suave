module HttpEmbedded

open Fuchu

open Suave
open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Http.Embedded

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let embedded_resources =
  let runWith' = runWith defaultConfig

  testList "test Embedded.browse" [
      testCase "200 OK returns embedded file" <| fun _ ->
        Assert.Equal("expecting 'Hello World!'", "Hello World!", runWith' browseDefaultAsssembly |> req HttpMethod.GET "/embedded-resource.txt" None)
    ]