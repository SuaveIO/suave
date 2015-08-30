module Suave.Tests.Program

open Fuchu

[<EntryPoint>]
let main args =
  //defaultMainThisAssembly args
  Tests.run Suave.Tests.Owin.endToEnd