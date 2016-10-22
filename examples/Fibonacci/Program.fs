open Suave
open Suave.Http
open Suave.Filters
open Suave.Successful

open System.Net

/// Inspired by https://news.ycombinator.com/item?id=3067403

let fib n =
  let rec loop a b i = async {
    if i > n then
      return b
    else
      return! loop b (a + b) (i + 1I)
  }
  async {
    if n = 1I || n = 2I then
      return 1I
    else
      return! loop 1I 1I 3I
  }

let app =
  pathScan "/%d" (fun (n : int) -> fun x -> async{ let! r = fib (bigint n) in return! OK (r.ToString()) x })

let config =
  { defaultConfig with
     bindings = [ HttpBinding.create HTTP IPAddress.Loopback 3000us ] }

[<EntryPoint>]
let main _ =
  startWebServer config app
  0
