module Suave.Tests.FormDataParsing

open HttpFs
open HttpFs.Client
open Expecto
open System
open System.IO
open System.Reflection
open Hopac
open Suave
open Suave.Utils
open Suave.Logging
open Suave.Operators
open Suave.Filters
open Suave.RequestErrors
open Suave.Testing
open Suave.Tests.TestUtilities

let app =
  choose
    [ POST
      >=> choose [
          path "/gifs/echo"
              >=> Writers.setMimeType "image/gif"
              >=> warbler (fun ctx ->
                  let file = ctx.request.files.Head
                  //printfn "||| in suave, handing over to sendFile, file %s len %d"
                  //        file.tempFilePath (FileInfo(file.tempFilePath).Length)
                  Files.sendFile file.tempFilePath false)
          NOT_FOUND "Nope."
      ]
    ]

let pathOf relativePath =
  let here = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
  Path.Combine(here, relativePath)

[<Tests>]
let tests (cfg : SuaveConfig) =
  let runWithConfig = runWith cfg
  let uriFor (res : string) =
    SuaveConfig.firstBindingUri cfg res ""

  let postTo res = Request.create Post (uriFor res) |> Request.keepAlive false

  testCase "can send/receive" <| fun _ ->
    let ctx = runWithConfig app
    try
      use fs = File.OpenRead (pathOf "regressions/pix.gif")
      let file = "pix.gif", ContentType.create("image", "gif"), StreamData fs

      //printfn "--- get response"
      let data =
        postTo "gifs/echo"
        |> Request.body (BodyForm [ FormFile ("img", file) ])
        |> Request.responseAsBytes
        |> run

      fs.Seek(0L, SeekOrigin.Begin) |> ignore

      use ms = new MemoryStream()
      ms.Write(data, 0, data.Length)
      ms.Seek(0L, SeekOrigin.Begin) |> ignore
      Expect.streamsEqual ms fs "the input should eq the echoed data"
    finally
      disposeContext ctx
      ()
