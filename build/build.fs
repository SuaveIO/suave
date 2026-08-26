open Fake
open Fake.Core
open Fake.DotNet
open Fake.IO
open Fake.IO.Globbing.Operators
open Fake.IO.FileSystemOperators
open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open Fake.Tools
open Fake.Core.TargetOperators

Console.OutputEncoding <- Encoding.UTF8

let releaseNotesPath = "RELEASE_NOTES.md"
let semVerPath = ".semver"
let nugetSource = "https://api.nuget.org/v3/index.json"
let pkgDir = Path.GetFullPath "./pkg"

/// `.semver` is the single source of truth for the version we publish. It is a
/// small YAML document written by the `semver` gem: `:major: 3`, `:minor: 4`, …
let loadSemVerFile () =
  let fields =
    File.ReadAllLines semVerPath
    |> Array.choose (fun line ->
      let m = Regex.Match(line, @"^\s*:(?<key>\w+):\s*(?<value>.*?)\s*$")
      if m.Success then Some (m.Groups.["key"].Value, m.Groups.["value"].Value.Trim([| '\''; '"' |]))
      else None)
    |> Map.ofArray

  let field name =
    match Map.tryFind name fields with
    | Some value -> value
    | None -> failwithf "%s is missing the ':%s:' field." semVerPath name

  sprintf "%s.%s.%s" (field "major") (field "minor") (field "patch")
  + (if String.isNullOrWhiteSpace (field "special") then "" else "-" + field "special")
  + (if String.isNullOrWhiteSpace (field "metadata") then "" else "+" + field "metadata")
  |> SemVer.parse

/// The topmost `## New in vX.Y.Z` entry, skipping the `## Unreleased` section.
let loadLatestVersionedRelease () =
  let lines = File.ReadAllLines releaseNotesPath
  let releaseStart =
    lines
    |> Array.tryFindIndex (fun line -> line.StartsWith("## New in v", StringComparison.Ordinal))

  match releaseStart with
  | Some index -> lines |> Seq.skip index |> ReleaseNotes.parse
  | None -> failwithf "Unable to find a versioned release entry in %s." releaseNotesPath

/// The version and notes to release, having checked that every source agrees.
let loadRelease () =
  let semVer = loadSemVerFile ()
  let notes = loadLatestVersionedRelease ()
  if string notes.SemVer <> string semVer then
    failwithf
      "Version mismatch: %s says %O but the latest '## New in v…' entry in %s is %O."
      semVerPath semVer releaseNotesPath notes.SemVer
  semVer, notes

let releaseTag (semVer: SemVerInfo) = sprintf "v%O" semVer

/// On a tag build GitHub sets GITHUB_REF to `refs/tags/vX.Y.Z`; RELEASE_TAG lets
/// you exercise the same check locally. Absent both, there is nothing to check.
let tagUnderRelease () =
  match Environment.environVarOrNone "RELEASE_TAG" with
  | Some tag when not (String.isNullOrWhiteSpace tag) -> Some (tag.Trim())
  | _ ->
    Environment.environVarOrNone "GITHUB_REF"
    |> Option.filter (fun r -> r.StartsWith("refs/tags/", StringComparison.Ordinal))
    |> Option.map (fun r -> r.Substring("refs/tags/".Length))

let initTargets () =
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

  Target.create "CheckVersion" <| fun _ ->
    let semVer, notes = loadRelease ()
    match tagUnderRelease () with
    | Some tag when tag <> releaseTag semVer ->
      failwithf "Tag '%s' does not match %s; expected '%s'." tag semVerPath (releaseTag semVer)
    | Some tag -> Trace.tracefn "Tag %s matches %s." tag semVerPath
    | None -> Trace.tracefn "No release tag in the environment; only checking %s against %s." semVerPath releaseNotesPath
    Trace.tracefn "Release v%O:" semVer
    notes.Notes |> List.iter (Trace.tracefn "  %s")

  Target.create "AsmInfo" <| fun _ ->
    let semVer, _ = loadRelease ()
    let assemblyVersion = sprintf "%d.%d.%d" semVer.Major semVer.Minor semVer.Patch
    projects |> Seq.iter (fun project ->
      let dir = Path.GetDirectoryName project
      let name = Path.GetFileNameWithoutExtension project
      let filePath = dir </> "AssemblyInfo.fs"
      AssemblyInfoFile.createFSharp filePath
        [ AssemblyInfo.Title name
          AssemblyInfo.Description "Suave — a smooth, open source, F# web server."
          AssemblyInfo.Version assemblyVersion
          AssemblyInfo.FileVersion assemblyVersion
          AssemblyInfo.InformationalVersion (string semVer)
          AssemblyInfo.Metadata ("Commit", Git.Information.getCurrentHash ())
        ])

  Target.create "Build" <| fun _ ->
    DotNet.build (fun args -> { args with MSBuildParams = { MSBuild.CliArguments.Create() with DisableInternalBinLog = true }}) "Suave.sln"

  Target.create "Tests" <| fun _ ->
    let path = "src" </> "Suave.Tests"
    let res = DotNet.exec id "run" (sprintf "-c Release --framework net10.0 --project %s -- --summary --sequenced" path)
    if not res.OK then
      res.Errors |> Seq.iter (eprintfn "%s")
      failwith "Tests failed."

  // Requires `httperf` installed on the server (only linux atm)
  Target.create "Load" <| fun _ ->
    let path = "examples" </> "Pong"
    let res = DotNet.exec id "run" (sprintf "-c Release --framework net10.0 --project %s" path)
    if not res.OK then
      res.Errors |> Seq.iter (eprintfn "%s")
      failwith "Tests failed."

  Target.create "Docs" <| fun _ ->
    let script = Path.GetFullPath ("scripts" </> "generate-api-docs.sh")
    let result =
      CreateProcess.fromRawCommand "bash" [ script ]
      |> CreateProcess.withWorkingDirectory (Path.GetFullPath ".")
      |> Proc.run
    if result.ExitCode <> 0 then
      failwithf "API docs generation failed with exit code %d" result.ExitCode

  Target.create "Pack" <| fun _ ->
    let semVer, notes = loadRelease ()
    Shell.cleanDir pkgDir
    let props (project: string) (p: Paket.PaketPackParams) =
      // paket is a local dotnet tool here; without this Fake looks for paket.exe.
      { p with ToolType = ToolType.CreateLocalTool()
               OutputPath = pkgDir
               IncludeReferencedProjects = true
               Symbols = true
               ProjectUrl = "https://suave.io"
               Version = string semVer
               WorkingDir = Path.GetDirectoryName project
               ReleaseNotes = String.Join("\n", notes.Notes)
               TemplateFile = "paket.template" }

    projects
    |> Seq.iter (fun project -> DotNet.Paket.pack (props project))

    // Side outputs the release workflow feeds to `gh release create`.
    File.WriteAllText(pkgDir </> "version.txt", string semVer)
    File.WriteAllLines(pkgDir </> "release-notes.md", notes.Notes |> List.map (sprintf "- %s"))

  Target.create "Push" <| fun _ ->
    // NUGET_API_KEY is what the nuget.org Trusted Publishing login hands back;
    // NUGET_KEY is the legacy long-lived key, kept so a manual push still works.
    let apiKey =
      [ "NUGET_API_KEY"; "NUGET_KEY" ]
      |> List.tryPick (Environment.environVarOrNone >> Option.filter (String.isNullOrWhiteSpace >> not))
      |> Option.defaultWith (fun () -> failwith "Neither NUGET_API_KEY nor NUGET_KEY is set.")

    // `*.nupkg` also matches the `.symbols.nupkg` paket emits alongside each
    // package; nuget.org takes symbols as `.snupkg` only, so filter those out.
    let packages =
      !! (pkgDir </> "*.nupkg")
      -- (pkgDir </> "*.symbols.nupkg")
      |> List.ofSeq

    if List.isEmpty packages then
      failwithf "No packages found in %s; run the Pack target first." pkgDir

    packages |> List.iter (fun package ->
      Trace.tracefn "Pushing %s" (Path.GetFileName package)
      let result =
        CreateProcess.fromRawCommand
          "dotnet"
          [ "nuget"; "push"; package
            "--source"; nugetSource
            "--api-key"; apiKey
            "--skip-duplicate" ]
        // Keeps the api key out of the build log.
        |> CreateProcess.disableTraceCommand
        |> Proc.run
      if result.ExitCode <> 0 then
        failwithf "Failed to push %s (exit code %d)." (Path.GetFileName package) result.ExitCode)

  // Cuts the release by pushing the tag; the Release workflow takes it from there.
  Target.create "Tag" <| fun _ ->
    let semVer, _ = loadRelease ()
    let tag = releaseTag semVer

    if Git.Information.isCleanWorkingCopy "" |> not then
      failwith "The working copy has uncommitted changes; commit them before tagging."

    let gitOwnerName = "SuaveIO/suave"
    let remote =
      Git.CommandHelper.getGitResult "" "remote -v"
      |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
      |> function None -> "git@github.com:SuaveIO/suave.git"
                | Some s -> s.Split().[0]

    Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")
    Git.Branches.tag "" tag
    Git.Branches.pushTag "" remote tag
    Trace.tracefn "Pushed %s to %s; the Release workflow publishes from here." tag remote

  "Clean"
    ==> "Restore"
    ==> "AsmInfo"
    ==> "Build"
    ==> "Tests"
    ==> "Load"

  "Build"
    ==> "Docs"

  // A release is built from a clean tree, tested, and only then packed.
  "CheckVersion"
    ==> "Pack"

  "Tests"
    ==> "Pack"
    ==> "Push"

  "CheckVersion"
    ==> "Tag"

[<EntryPoint>]
let main argv =
    argv
    |> Array.toList
    |> Context.FakeExecutionContext.Create false "build.fsx"
    |> Context.RuntimeContext.Fake
    |> Context.setExecutionContext
    initTargets ()
    try
      Target.runOrDefault "Tests"
      0
    with e ->
      // Fail the build with a readable message instead of an unhandled exception.
      Trace.traceError e.Message
      1
