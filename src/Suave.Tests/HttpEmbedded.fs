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

  testList "test embedded resources" [
      testCase "200 OK returns embedded file" <| fun _ ->
        let app = browseDefaultAsssemblyManifest None None
        let actual = app |> runWithConfig |> req HttpMethod.GET "/embedded-resource.txt" None
        Expect.equal actual "Hello World!" "expecting 'Hello World!'"

      testCase "200 OK returns embedded files in wwwroot" <| fun _ ->
        let app = browseDefaultAsssemblyManifest None (Some "wwwroot")

        let assets = [
          ("/css/app.css"            , "/* app.css */")
          ("/css/bootstrap.min.css"  , "/* bootstrap.min.css */")
          ("/js/app.js"              , "/* app.js */")
          ("/js/lib.js"              , "/* lib.js */")
          ("/js/vendor/jquery.min.js", "/* jquery.min.js */")
          ("/js/vendor/lodash.min.js", "/* lodash.min.js */")
        ]

        for path, content in assets do
          let actual = app |> runWithConfig |> req HttpMethod.GET path None
          Expect.equal actual content $"expecting '{content}'"
    ]
