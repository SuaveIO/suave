#!/usr/bin/env fsharpi
#r @"packages/build/FAKE/tools/FakeLib.dll"

open System
open Fake

Environment.CurrentDirectory <- __SOURCE_DIRECTORY__

let configuration = environVarOrDefault "Configuration" "Release"
let release = IO.File.ReadAllLines "RELEASE_NOTES.md" |> ReleaseNotesHelper.parseReleaseNotes
let description = "Suave is a simple web development F# library providing a lightweight web server and a set of combinators to manipulate route flow and task composition."
let tags = "http socket applicative functor web-server http-client"
let authors = "Ademar Gonzalez, Henrik Feldt"
let owners = "Ademar Gonzalez, Henrik Feldt"
let projectUrl = "https://github.com/suaveio/suave"
let iconUrl = "https://raw.githubusercontent.com/SuaveIO/resources/master/images/head_trans.png"
let licenceUrl = "htps://github.com/haf/expecto/blob/master/LICENSE"
let copyright = "Copyright \169 (c) Ademar Gonzalez, Henrik Feldt, contributors 2018"

let mutable dotnetExePath = "dotnet"

//Target "InstallDotNetCore" <| fun _ ->
//  dotnetExePath <- DotNetCli.InstallDotNetSDK "2.1.4"

Target "Clean" (fun _ -> !!"./**/bin/" ++ "./**/obj/" |> CleanDirs)

let projects =
  [ "Suave"
    "Suave.DotLiquid"
    "Suave.LibUv"
    "Suave.Testing"
  ]

open AssemblyInfoFile

Target "AssemblyInfo" <| fun _ ->
  projects
  |> List.iter (fun proj ->
    [ Attribute.Title proj
      Attribute.Product proj
      Attribute.Copyright copyright
      Attribute.Description description
      Attribute.Version release.AssemblyVersion
      Attribute.FileVersion release.AssemblyVersion
    ]
    |> CreateFSharpAssemblyInfo (sprintf "src/%s/AssemblyInfo.fs" proj)
  )

Target "PaketFiles" (fun _ ->
  FileHelper.ReplaceInFiles
    ["namespace Logary.Facade","namespace Suave.Logging"]
    ["paket-files/logary/logary/src/Logary.Facade/Facade.fs"]

  for grp in [ "/"; "/docs"; "/examples" ] do
    FileHelper.ReplaceInFiles
      ["module YoLo","module internal Suave.Utils.YoLo"]
      [sprintf "paket-files%s/haf/YoLo/YoLo.fs" grp]
)

Target "ProjectVersion" <| fun _ ->
  projects
  |> List.map (fun p -> sprintf "src/%s/%s.fsproj" p p)
  |> List.iter (fun file ->
      XMLHelper.XmlPoke
        file
        "Project/PropertyGroup/Version/text()"
        release.NugetVersion)

let build project =
  DotNetCli.Build <| fun p ->
  { p with
      ToolPath = dotnetExePath
      Configuration = configuration
      Project = project
  }

Target "BuildTest" <| fun _ ->
  build "src/Suave.Tests/Suave.Tests.fsproj"

let run f =
  DotNetCli.RunCommand (fun p -> f { p with ToolPath = dotnetExePath })

Target "RunTest" <| fun _ ->
  run id (sprintf "src/Suave.Tests/bin/%s/netcoreapp2.0/Suave.Tests.dll --summary" configuration)

  Shell.Exec (
    sprintf "src/Suave.Tests/bin/%s/net461/Suave.Tests.exe" configuration,
    "--summary")
  |> fun r -> if r<>0 then failwith "Suave.Tests.exe failed"

Target "Pack" <| fun _ ->
  let packParameters name =
    [ "--no-build"
      "--no-restore"
      sprintf "/p:Title=\"%s\"" name
      "/p:PackageVersion=" + release.NugetVersion
      sprintf "/p:Authors=\"%s\"" authors
      sprintf "/p:Owners=\"%s\"" owners
      "/p:PackageRequireLicenseAcceptance=false"
      sprintf "/p:Description=\"%s\"" description
      sprintf "/p:PackageReleaseNotes=\"%O\"" ((toLines release.Notes).Replace(",",""))
      sprintf "/p:Copyright=\"%s\"" copyright
      sprintf "/p:PackageTags=\"%s\"" tags
      sprintf "/p:PackageProjectUrl=\"%s\"" projectUrl
      sprintf "/p:PackageIconUrl=\"%s\"" iconUrl
      sprintf "/p:PackageLicenseUrl=\"%s\"" licenceUrl
    ]
    |> String.concat " "

  for proj, pars in projects |> List.map (fun p -> p, packParameters p) do
    run id (sprintf "pack %s/%s.fsproj -c %s -o ../bin %s" proj proj configuration pars)

Target "Push" <| fun _ ->
  Paket.Push (fun p -> { p with WorkingDir = "bin" })

#load "paket-files/build/fsharp/FAKE/modules/Octokit/Octokit.fsx"
Target "Release" <| fun _ ->
  let gitOwner, gitName, gitOwnerName =
    "SuaveIO", "Suave", "SuaveIO/Suave"

  let remote =
    Git.CommandHelper.getGitResult "" "remote -v"
    |> Seq.tryFind (fun s -> s.EndsWith "(push)" && s.Contains gitOwnerName)
    |> function None -> ("ssh://github.com/"+gitOwnerName) | Some s -> s.Split().[0]

  Git.Staging.StageAll ""
  Git.Commit.Commit "" (sprintf "Release %s" release.NugetVersion)
  Git.Branches.pushBranch "" remote (Git.Information.getBranchName "")

  Git.Branches.tag "" release.NugetVersion
  Git.Branches.pushTag "" remote release.NugetVersion

  let user, pw =
    getUserInput "Github Username: ",
    getUserPassword "Github Password: "

  Octokit.createClient user pw
  |> Octokit.createDraft gitOwner gitName release.NugetVersion
      (Option.isSome release.SemVer.PreRelease) release.Notes
  |> Octokit.releaseDraft
  |> Async.RunSynchronously

Target "All" ignore

"Clean"
  // ==> "InstallDotNetCore"
  ==> "AssemblyInfo"
  ==> "PaketFiles"
  ==> "ProjectVersion"
  ==> "BuildTest"
  ==> "RunTest"
  ==> "Pack"
  ==> "All"
  ==> "Push"
  ==> "Release"

RunTargetOrDefault "All"