#!/usr/bin/env fsharpi

#r "paket:
nuget Fake.DotNet.Cli //"
open Fake.DotNet

// Lazily install DotNet SDK in the correct version if not available
let install = lazy DotNet.install DotNet.Release_2_1_105

// Define general properties across various commands (with arguments)
let inline withWorkDir wd =
  DotNet.Options.lift install.Value
  >> DotNet.Options.withWorkingDirectory wd

// Set general properties without arguments
let inline dotnetSimple arg = DotNet.Options.lift install.Value arg

DotNet.restore dotnetSimple "Suave.sln"
DotNet.exec dotnetSimple "build" "Suave.sln"
