module Suave.Tests.Program

open Suave.Http
open Suave.Web
open Suave.Logging
open Suave.LibUv
open ExpectoExtensions

open System

#if NETCOREAPP2_0
open System.Runtime.InteropServices
#endif

[<EntryPoint>]
let main args =

  let arch s = if s then "64-bit" else "32-bit"

  Console.WriteLine("OSVersion: {0}; running {1} process on {2} operating system."
    , Environment.OSVersion.ToString()
    , arch Environment.Is64BitProcess
    , arch Environment.Is64BitOperatingSystem)

  let testConfig =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 9001 ]
        logger   = Targets.create Warn [| "Suave"; "Tests" |] }

  let mutable firstRun = 0
  let runDefaultEngine() =
    Console.WriteLine "Running tests with default TCP engine."
    firstRun <- defaultMainThisAssemblyWithParam testConfig args
    Console.WriteLine "Done."

  #if NETCOREAPP2_0
  if not (RuntimeInformation.IsOSPlatform(OSPlatform.Windows)) then
    Console.WriteLine "Skipping default TCP engine tests for non Windows platforms."
  else
    runDefaultEngine()
  #else
  runDefaultEngine()
  #endif

  if firstRun <> 0 then
    firstRun
  else
    Console.WriteLine "Running tests with LibUv TCP engine."
    let libUvConfig = { testConfig with tcpServerFactory = LibUvServerFactory() }
    let r = defaultMainThisAssemblyWithParam libUvConfig args
    Console.WriteLine "Done."
    r
