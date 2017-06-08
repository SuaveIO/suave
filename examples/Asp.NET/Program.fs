
open Suave
open Logary.Facade
open System.IO
open System.Net

let config =
  { defaultConfig with
     logger = Targets.create Verbose [||] }

[<EntryPoint>]
let main _ =
  let appHost = Xsp.createApplication <| Directory.GetCurrentDirectory()
  startWebServer defaultConfig (Xsp.run appHost)
  0
