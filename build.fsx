#!/usr/bin/env fsharpi

#r "paket: groupref Build //"
open Fake.Core
#load ".fake/build.fsx/intellisense.fsx"
open Fake
open Fake.Core
open Fake.DotNet
open Fake.DotNet.Testing
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open Fake.Tools.Git
open System
open System.IO
open System.Text
Console.OutputEncoding <- Encoding.UTF8

let release = ReleaseNotes.load "RELEASE_NOTES.md"
let version = release.SemVer

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

Target.create "Clean" <| fun _ ->
  !! "src/**/bin"
  ++ "src/**/obj"
  |> Shell.CleanDirs

Target.create "Restore" <| fun _ ->
  DotNet.restore dotnetSimple "Suave.sln"

Target.create "AsmInfo" <| fun _ ->
  projects |> Seq.iter (fun project ->
    let dir = Path.GetDirectoryName project
    let name = Path.GetFileNameWithoutExtension project
    let filePath = dir </> "AssemblyInfo.fs"
    AssemblyInfoFile.createFSharp filePath
      [ AssemblyInfo.Title name
        AssemblyInfo.Description "Suave — a smooth, open source, F# web server."
        AssemblyInfo.Version release.AssemblyVersion
        AssemblyInfo.FileVersion release.AssemblyVersion
        AssemblyInfo.Metadata ("Commit", Information.getCurrentHash ())
      ])

Target.create "Replace" <| fun _ ->
  Shell.ReplaceInFiles
    [ "Logary.Facade", "Suave.Logging" ]
    (!! "paket-files/logary/logary/src/Logary.Facade/Facade.fs")

Target.create "Build" <| fun _ ->
  DotNet.build dotnetSimple "Suave.sln"

let testNetCoreDir = "src" </> "Suave.Tests" </> "bin" </> "Release" </> "netcoreapp2.0"

Target.create "Test" <| fun _ ->
  let res = DotNet.exec id "run" (testNetCoreDir </> "Suave.Tests.dll")
  if not res.OK then
    res.Errors |> Seq.iter (eprintfn "%s")
    failwith "Tests failed."

Target.create "Pack" <| fun _ ->
  projects |> Seq.iter (fun project ->
    DotNet.Paket.pack (fun opts -> { opts with WorkingDir = Path.GetDirectoryName project }))

Target.create "Release" <| fun _ ->
  printfn "TODO"
  ()

// Dependencies
open Fake.Core.TargetOperators

"Clean"
  ==> "Restore"
  ==> "AsmInfo"
  ==> "Replace"
  ==> "Build"
  ==> "Test"
  ==> "Pack"
  ==> "Release"

Target.runOrDefault "Test"