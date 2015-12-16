module Program

open System
open System.Net

open Suave
open Suave.Logging
open Suave.Web
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Writers
open Suave.Http.Files
open Suave.Http.Successful
open Suave.Http.CORS
open Suave.Types
open Suave.State.CookieStateStore
open Suave.Utils

let logger = Loggers.ConsoleWindowLogger LogLevel.Verbose

let corsConfig = { defaultCORSConfig with allowedUris = [ "http://localhost:8085" ] }

let app =
    choose [ 
        GET >>= path "/hello" >>= cors corsConfig >>= OK "CORS request accepted."
    ] >>= log logger logFormat

[<EntryPoint>]
let main argv =
  startWebServer defaultConfig app
  0