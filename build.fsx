#!/usr/bin/env fsharpi

#r "paket: groupref Build //"
#load ".fake/build.fsx/intellisense.fsx"
open Fake
open Fake.Core
open Fake.DotNet
open Fake.Api
open Fake.Tools
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open System
open System.IO
open System.Text

Console.OutputEncoding <- Encoding.UTF8

// brew install libuv

let release = ReleaseNotes.load "RELEASE_NOTES.md"
let Configuration = Environment.environVarOrDefault "CONFIGURATION" "Release"

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
  >> DotNet.Options.withCustomParams (Some (sprintf "/p:Configuration=%s" Configuration))

// Set general properties without arguments
let inline dotnetSimple arg = DotNet.Options.lift install.Value arg

let projects =
  !! "src/**/Suave.*.fsproj"

Target.create "Clean" <| fun _ ->
  !! "src/**/bin"
  ++ "src/**/obj"
  |> Shell.cleanDirs

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
        AssemblyInfo.Metadata ("Commit", Git.Information.getCurrentHash ())
      ])

Target.create "Replace" <| fun _ ->
  Shell.replaceInFiles
    [ "Logary.Facade", "Suave.Logging" ]
    (!! "paket-files/logary/logary/src/Logary.Facade/Facade.fs")

Target.create "Build" <| fun _ ->
  DotNet.build dotnetSimple "Suave.sln"

Target.create "Tests" <| fun _ ->
  let path = "src" </> "Suave.Tests"
  let res = DotNet.exec id "run" (sprintf "--framework netcoreapp2.0 --project %s -- --summary --sequenced" path)
  if not res.OK then
    res.Errors |> Seq.iter (eprintfn "%s")
    failwith "Tests failed."

Target.create "Pack" <| fun _ ->
  projects |> Seq.iter (fun project ->
    DotNet.Paket.pack (fun opts -> { opts with WorkingDir = Path.GetDirectoryName project }))

Target.create "CheckEnv" <| fun _ ->
  ignore (Environment.environVarOrFail "GITHUB_TOKEN")

Target.create "Release" <| fun _ ->
  let gitOwner, gitName = "logary", "logary"
  let gitOwnerName = gitOwner + "/" + gitName
  let remote =
      Git.CommandHelper.getGitResult "" "remote -v"
      |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
      |> function None -> "git@github.com:logary/logary.git"
                | Some s -> s.Split().[0]

  Git.Staging.stageAll ""
  Git.Commit.exec "" (sprintf "Release of v%O" release.SemVer)
  Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

  let tag = sprintf "v%O" release.SemVer
  Git.Branches.tag "" tag
  Git.Branches.pushTag "" remote tag

  GitHub.createClientWithToken (Environment.environVarOrFail "GITHUB_TOKEN")
  |> GitHub.draftNewRelease gitOwner gitName release.NugetVersion
      (Option.isSome release.SemVer.PreRelease) release.Notes
  |> GitHub.publishDraft
  |> Async.RunSynchronously

// Dependencies
open Fake.Core.TargetOperators

"CheckEnv"
  ==> "Release"

"Clean"
  ==> "Restore"
  ==> "AsmInfo"
  ==> "Replace"
  ==> "Build"
  ==> "Tests"
  ==> "Pack"
  ==> "Release"

Target.runOrDefault "Tests"
