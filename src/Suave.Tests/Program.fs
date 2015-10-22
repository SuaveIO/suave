module Suave.Tests.Program

open Suave.Types
open Suave.Web
open Suave.Logging

open FuchuExtensions

[<EntryPoint>]
let main args =
  let defaultConfig =
    { defaultConfig with logger = Loggers.saneDefaultsFor LogLevel.Warn}

  let firstRun = defaultMainThisAssemblyWithParam defaultConfig args

  if firstRun <> 0 then
    firstRun
  else
    let libUvConfig = { defaultConfig with tcpServerFactory = Suave.LibUv.Tcp.LibUvServerFactory() }
    defaultMainThisAssemblyWithParam libUvConfig args