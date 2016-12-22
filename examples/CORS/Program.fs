module Program

open System
open System.Net

open Suave
open Suave.Logging
open Suave.Filters
open Suave.Writers
open Suave.Files
open Suave.Successful
open Suave.CORS
open Suave.State.CookieStateStore
open Suave.Utils
open Suave.Operators

let logger = Targets.create Verbose [||]

let corsConfig = { defaultCORSConfig with allowedUris = InclusiveOption.Some [ "http://localhost:8085" ] }

let app =
    choose [
        GET >=> path "/hello" >=> cors corsConfig >=> OK "CORS request accepted."
    ] >=> logStructured logger logFormatStructured

[<EntryPoint>]
let main argv =
  startWebServer defaultConfig app
  0
