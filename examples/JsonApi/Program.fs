module Program

/// This is a demo showcasing how one can respond with JSON to requests

open Chiron
open Suave
open Chiron.Operators
open Suave.Operators
open Suave
open Suave.Successful
open Suave.Filters

type Monkey =
  { sound : string
    height : decimal }

  static member ToJson (x : Monkey) =
    Json.write "sound" x.sound
    *> Json.write "height" x.height

let app : WebPart =
  GET
    >=> path "/monkey"
    >=> OK (Json.format (Json.serialize { sound = "ooh"; height = 1.53m }))
    >=> Writers.setMimeType "application/json; charset=utf-8"

[<EntryPoint>]
let main argv =
  startWebServer defaultConfig app
  0

(*
➜  suave git:(master) ✗ curl http://127.0.0.1:8083/monkey --silent | jq .
{
  "height": 1.53,
  "sound": "ooh"
}
*)