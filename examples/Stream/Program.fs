module Program

open System.IO

open Suave
open Suave.Filters
open Suave.Stream
open Suave.Operators

let makeStream =
    async {
        let fileStream = File.Open("./kandinsky-composition-8.jpg", FileMode.Open, FileAccess.Read, FileShare.Read)

        return fileStream :> Stream
    }

let app =
    choose [
        GET >=> path "/art" >=> Writers.setMimeType "image/jpeg" >=> okStream makeStream
    ]

[<EntryPoint>]
let main argv =
  startWebServer defaultConfig app
  0
