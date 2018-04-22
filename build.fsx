#!/usr/bin/env fsharpi

#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake.Core
open Fake.DotNet
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Tools
open System.IO


let version = "1.0.0" // ??
let Release_2_1_105 (options: DotNet.CliInstallOptions) =
    { options with
        InstallerOptions = (fun io ->
            { io with
                Branch = "release/2.1.1xx"
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

let projects =
  !! "src/**/Suave.*.fsproj"

Target.create "Restore" <| fun _ ->
  DotNet.restore dotnetSimple "Suave.sln"

Target.create "AsmInfo" <| fun _ ->
  let commitHash = Git.Information.getCurrentHash()
  projects |> Seq.iter (fun project ->
    let dir = Path.GetDirectoryName project
    let name = Path.GetFileNameWithoutExtension project
    let filePath = dir </> "AssemblyInfo.fs"
    AssemblyInfoFile.createFSharp filePath
      [AssemblyInfo.Title name
       AssemblyInfo.Description "Suave — a smooth, open source, F# web server."
       AssemblyInfo.Version version
       AssemblyInfo.FileVersion version])

Target.create "Replace" <| fun _ ->
  // TODO: replace Logary.Facade with Suave.Logging
  ()

Target.create "Build" <| fun _ ->
  DotNet.build dotnetSimple "Suave.sln"

Target.runOrDefault "Build"