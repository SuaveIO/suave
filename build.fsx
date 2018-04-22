#!/usr/bin/env fsharpi

#r "paket: groupref Build
nuget Fake.DotNet.Cli //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.DotNet

let Release_2_1_105 (options: DotNet.CliInstallOptions) =
    { options with
        InstallerOptions = (fun io ->
            { io with
                Branch = "release/2.1.105"
            })
        Channel = None
        Version = DotNet.Version "2.1.105"
    }

// Lazily install DotNet SDK in the correct version if not available
let install = lazy DotNet.install Release_2_1_105

// Define general properties across various commands (with arguments)
let inline withWorkDir wd =
  DotNet.Options.lift install.Value
  >> DotNet.Options.withWorkingDirectory wd

// Set general properties without arguments
let inline dotnetSimple arg = DotNet.Options.lift install.Value arg

DotNet.restore dotnetSimple "Suave.sln"
DotNet.exec dotnetSimple "build" "Suave.sln"
