module HttpEmbedded

open Expecto
open Suave
open Suave.Successful
open Suave.Embedded
open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let embedded_resources cfg =
  let runWithConfig = runWith cfg

  testList "test Embedded.browse" [
      testCase "200 OK returns embedded file" <| fun _ ->
        let actual = runWithConfig browseDefaultAsssembly |> req HttpMethod.GET "/embedded-resource.txt" None
        Expect.equal actual "Hello World!" "expecting 'Hello World!'"
    ]
