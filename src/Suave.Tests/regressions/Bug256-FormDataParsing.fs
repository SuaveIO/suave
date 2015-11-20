module Suave.Tests.FormDataParsing 

open HttpFs
open HttpFs.Client
open Fuchu
open System
open System.IO
open System.Reflection

open Suave
open Suave.Web
open Suave.Utils
open Suave.Logging
open Suave.Http
open Suave.Http.Operators
open Suave.Http.Applicatives
open Suave.Http.RequestErrors
open Suave.Testing
open Suave.Tests.TestUtilities  

let app =
  choose
    [ POST
      >>= choose [
          path "/gifs/echo"
              >>= Writers.setMimeType "image/gif"
              >>= warbler (fun ctx ->
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
let tests =
  let config = defaultConfig //{ defaultConfig with logger = Loggers.ConsoleWindowLogger(LogLevel.Verbose) }
  let runWithConfig = runWith config
  let uriFor (res : string) = Uri (sprintf "http://localhost:8083/%s" (res.TrimStart('/')))
  let postTo res = createRequest Post (uriFor res) |> withKeepAlive false

  testCase "can send/receive" <| fun _ ->
    let ctx = runWithConfig app
    try
      use fs = File.OpenRead (pathOf "regressions/pix.gif")
      let file = "pix.gif", ContentType.Create("image", "gif"), StreamData fs

      //printfn "--- get response"
      let data =
        postTo "gifs/echo"
        |> withBody (BodyForm [ FormFile ("img", file) ])
        |> Request.responseAsBytes
        |> Async.RunSynchronously

      fs.Seek(0L, SeekOrigin.Begin) |> ignore

      use ms = new MemoryStream()
      ms.Write(data, 0, data.Length)
      ms.Seek(0L, SeekOrigin.Begin) |> ignore
      Assert.StreamsEqual("the input should eq the echoed data", ms, fs)
    finally
      disposeContext ctx
      ()
