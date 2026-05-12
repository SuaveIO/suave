module Program


open Suave
open Suave.Filters
open Suave.Successful
open Suave.CORS
open Suave.Operators


let corsConfig = { defaultCORSConfig with allowedUris = InclusiveOption.Some [ "http://localhost:8085" ] }

let app =
    choose [
        GET >=> path "/hello" >=> cors corsConfig >=> OK "CORS request accepted."
    ]

[<EntryPoint>]
let main argv =
  startWebServer defaultConfig app
  0
