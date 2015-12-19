module Server

open Nessos.Argu
open Suave
open Suave.Http
open Suave.ServerErrors
open System.IO

type Arguments =
  | [<Mandatory>] Binding of string * int
  | [<Mandatory>] Home of string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Home _ -> "specify a working directory."
      | Binding _ -> "specify a listener '<hostname> <port>'"

let app : WebPart =
  choose [
    Files.browseFileHome "index.html"
    Files.browseHome
    request (fun r ->INTERNAL_ERROR (sprintf "No file found at path %s" r.url.AbsolutePath))
  ]

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create("""mono server.exe --binding <ip4oripv6> <port> --home <homedir>""")
  let parsed = parser.Parse argv
  let (host, port) = parsed.GetResult <@ Binding @>
  let home = parsed.GetResult <@ Home @> |> Path.GetFullPath

  printfn "Root path: %s" home

  let config =
    { defaultConfig with
        bindings = [ HttpBinding.mkSimple HTTP host port ]
        homeFolder = Some home }

  startWebServer config app
  0