
open Suave
open Suave.Types
open Suave.Web
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Logging

open System.IO
open System.Net

let config =
  { defaultConfig with
     logger = Loggers.ConsoleWindowLogger LogLevel.Verbose }

[<EntryPoint>]
let main _ =
  let appHost = Xsp.createApplication <| Directory.GetCurrentDirectory()
  startWebServer defaultConfig (Xsp.run appHost)
  0
