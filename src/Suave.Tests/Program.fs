module Suave.Tests.Program

open Suave.Web
open Suave.Logging
open FuchuExtensions

open System

[<EntryPoint>]
let main args =

  let arch s = if s then "64-bit" else "32-bit"

  Console.WriteLine("OSVersion: {0}; running {1} process on {2} operating system."
    , Environment.OSVersion.ToString()
    , arch Environment.Is64BitProcess
    , arch Environment.Is64BitOperatingSystem)

  let defaultConfig =
    { defaultConfig with logger = Loggers.saneDefaultsFor LogLevel.Warn}

  Console.WriteLine "Running tests with default TCP engine."
  let firstRun = defaultMainThisAssemblyWithParam defaultConfig args
  Console.WriteLine "Done."
  
  if firstRun <> 0 then
    firstRun
  else
    Console.WriteLine "Running tests with LibUv TCP engine."
    let libUvConfig = { defaultConfig with tcpServerFactory = Suave.LibUv.Tcp.LibUvServerFactory() }
    let r = defaultMainThisAssemblyWithParam libUvConfig args
    Console.WriteLine "Done."
    r