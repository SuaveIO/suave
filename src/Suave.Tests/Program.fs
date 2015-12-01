module Suave.Tests.Program

open Suave.Types
open Suave.Web
open Suave.Logging
open Suave.LibUv
open FuchuExtensions

open System

[<EntryPoint>]
let main args =

  let arch s = if s then "64-bit" else "32-bit"

  Console.WriteLine("OSVersion: {0}; running {1} process on {2} operating system."
    , Environment.OSVersion.ToString()
    , arch Environment.Is64BitProcess
    , arch Environment.Is64BitOperatingSystem)

  let testConfig =
    { defaultConfig with
        bindings = [ HttpBinding.mkSimple HTTP "127.0.0.1" 9001 ]
        logger   = Loggers.saneDefaultsFor LogLevel.Warn }

  Console.WriteLine "Running tests with default TCP engine."
  let firstRun = defaultMainThisAssemblyWithParam testConfig args
  Console.WriteLine "Done."

  if firstRun <> 0 then
    firstRun
  else
    Console.WriteLine "Running tests with LibUv TCP engine."
    let libUvConfig = { testConfig with tcpServerFactory = Tcp.LibUvServerFactory() }
    let r = defaultMainThisAssemblyWithParam libUvConfig args
    Console.WriteLine "Done."
    r