module Suave.Tests.Program

open Fuchu

[<EntryPoint>]
let main args =
  System.Net.ServicePointManager.Expect100Continue <- false
  defaultMainThisAssembly args