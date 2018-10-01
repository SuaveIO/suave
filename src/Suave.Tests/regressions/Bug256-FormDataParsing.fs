module Suave.Tests.FormDataParsing

open Expecto
open System
open System.IO
open System.Net.Http
open System.Reflection
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

  testCase "can send/receive" <| fun _ ->
    let ctx = runWithConfig app
    try
      use fs = File.OpenRead (pathOf "regressions/pix.gif")
      use formdata = new MultipartFormDataContent()
      let upload = new StreamContent(fs)
      upload.Headers.ContentType <- Headers.MediaTypeHeaderValue("image/gif")
      formdata.Add(upload,"file","pix.gif")
      use client = new HttpClient()
      let response =
        client.PostAsync(uriFor "gifs/echo",formdata).Result

      let data = response.Content.ReadAsByteArrayAsync().Result
      fs.Seek(0L, SeekOrigin.Begin) |> ignore

      use ms = new MemoryStream()
      ms.Write(data, 0, data.Length)
      ms.Seek(0L, SeekOrigin.Begin) |> ignore
      Expect.streamsEqual ms fs "the input should eq the echoed data"
    finally
      disposeContext ctx
      ()
