open Fake
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open System
open System.IO
open System.Text
open Fake.Tools
open Fake.Api
open Fake.Core.TargetOperators

Console.OutputEncoding <- Encoding.UTF8

let initTargets () =
  let release = ReleaseNotes.load "RELEASE_NOTES.md"

  let projects =
    !! "src/**/Suave*.fsproj"
    -- "src/*.Tests/*.fsproj"
    -- "src/*.IO/*.fsproj"

  Target.create "Clean" <| fun _ ->
    !! "src/**/bin"
    ++ "src/**/obj"
    |> Shell.cleanDirs

  Target.create "Restore" <| fun _ ->
    DotNet.restore (fun args -> { args with MSBuildParams = { MSBuild.CliArguments.Create() with DisableInternalBinLog = true }}) "Suave.sln"

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

  Target.create "Build" <| fun _ ->
    DotNet.build (fun args -> { args with MSBuildParams = { MSBuild.CliArguments.Create() with DisableInternalBinLog = true }}) "Suave.sln"

  Target.create "Tests" <| fun _ ->
    let path = "src" </> "Suave.Tests"
    let res = DotNet.exec id "run" (sprintf "-c Release --framework net9.0 --project %s -- --summary --sequenced" path)
    if not res.OK then
      res.Errors |> Seq.iter (eprintfn "%s")
      failwith "Tests failed."

  // Requires `httperf` installed on the server (only linux atm)
  Target.create "Load" <| fun _ ->
    let path = "examples" </> "Pong"
    let res = DotNet.exec id "run" (sprintf "-c Release --framework net9.0 --project %s" path)
    if not res.OK then
      res.Errors |> Seq.iter (eprintfn "%s")
      failwith "Tests failed."

  Target.create "Pack" <| fun _ ->
    let pkg = Path.GetFullPath "./pkg"
    let props (project: string) (p: Paket.PaketPackParams) =
      { p with OutputPath = pkg
               IncludeReferencedProjects = true
               Symbols = true
               ProjectUrl = "https://suave.io"
               Version = release.SemVer.ToString()
               WorkingDir = Path.GetDirectoryName project
               ReleaseNotes = String.Join("\n", release.Notes)
               //LicenseUrl = "https://opensource.org/licenses/Apache-2.0"
               TemplateFile = "paket.template" }

    projects
    |> Seq.iter (fun project -> DotNet.Paket.pack (props project))

  Target.create "Push" <| fun _ ->
    Paket.push (fun p ->
      { p with WorkingDir = "./pkg"
               ApiKey = Environment.environVarOrFail "NUGET_KEY" })

  Target.create "CheckEnv" <| fun _ ->
    ignore (Environment.environVarOrFail "NUGET_KEY")
    ignore (Environment.environVarOrFail "GITHUB_TOKEN")

  Target.create "Release" <| fun _ ->
    let gitOwner, gitName = "SuaveIO", "suave"
    let gitOwnerName = gitOwner + "/" + gitName
    let remote =
        Git.CommandHelper.getGitResult "" "remote -v"
        |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
        |> function None -> "git@github.com:SuaveIO/suave.git"
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

  "CheckEnv"
    ==> "Release"

  "Clean"
    ==> "Restore"
    ==> "AsmInfo"
    ==> "Build"
    ==> "Tests"
    ==> "Load"
    ==> "Pack"
    ==> "Release"

[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext
    initTargets ()
    Target.runOrDefault "Load"
    0
