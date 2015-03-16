
open Suave
open Suave.Types
open Suave.Web
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Logging

open System.Net

let config =
  { defaultConfig with
     logger = Loggers.ConsoleWindowLogger LogLevel.Verbose }

[<EntryPoint>]
let main _ =
  startWebServer defaultConfig Xsp.run
  0
