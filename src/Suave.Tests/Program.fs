module Suave.Tests.Program

open Suave.Types
open Suave.Web
open Suave.Logging

open FuchuExtensions

[<EntryPoint>]
let main args =
  let defaultConfig = { defaultConfig with logger = Loggers.saneDefaultsFor LogLevel.Warn}
  defaultMainThisAssemblyWithParam defaultConfig args |> ignore
