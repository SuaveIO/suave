module Suave.Tests.Program

open System
open Suave
open ExpectoExtensions

[<EntryPoint>]
let main args =

  let arch s = if s then "64-bit" else "32-bit"

  Console.WriteLine("OSVersion: {0}; running {1} process on {2} operating system."
    , Environment.OSVersion.ToString()
    , arch Environment.Is64BitProcess
    , arch Environment.Is64BitOperatingSystem)

  // Expecto redirects Console.Out and holds its own lock while flushing to the
  // real console, which on Unix locks Console.Out in turn. A Suave server
  // thread writing to Console.Out takes those locks in the opposite order and
  // can deadlock the run, so keep Suave's own messages off the console.
  Globals.messageWriter <- Some IO.TextWriter.Null

  let testConfig =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 9001 ]
        }

  defaultMainThisAssemblyWithParam testConfig args
