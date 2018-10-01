module Suave.IO.Program

open Argu
open Suave
open Suave.ServerErrors
open Suave.Logging
open Suave.Logging.Message
open System.IO
open System.Net

let logger = Log.create "Suave.IO"

type Arguments =
  | [<Mandatory>] Binding of string * int
  | [<Mandatory>] Home of string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Home _ -> "specify a working directory."
      | Binding _ -> "specify a listener '<hostname> <port>'"

let addExnLogging (fwp: 'a -> WebPart) =
  fun input ctx ->
    async {
      try
        return! fwp input ctx
      with e ->
        do logger.fatal (eventX "Unhandled {exception}" >> setField "exception" e)
        return! INTERNAL_ERROR "Unhandled internal exception" ctx
    }

let app: WebPart =
  choose [
    Files.browseHome
    Files.browseFileHome "index.html"
    request (fun r -> INTERNAL_ERROR (sprintf "No file found at path %s" r.url.AbsolutePath))
  ]

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create("""dotnet run -- --binding <ip4oripv6> <port> --home <homedir>""")
  let parsed = parser.Parse argv
  let (host, port) = parsed.GetResult <@ Binding @>
  let home = parsed.GetResult <@ Home @> |> Path.GetFullPath

  printfn "Root path: %s" home

  let config =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP host port ]
        homeFolder = Some home }

  startWebServer config app
  0