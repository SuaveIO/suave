namespace SuaveWebsite

open System
open System.IO
open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RequestErrors
open Suave.Files
open Suave.Writers
open Suave.Redirection

module Website =
  
  let contentDir = 
    let contentPath = Path.Combine(AppDomain.CurrentDomain.BaseDirectory, "content")
    if Directory.Exists contentPath then contentPath else "content"

  // Main app - serve static files with proper caching headers
  let app =
    choose [
      path "/" >=> Files.browseFileHome "index.html"
      path "/docs/" >=> Files.browseFileHome "docs/index.html"
      path "/docs/recipes/" >=> Files.browseFileHome "docs/recipes/index.html"
      path "/reference" >=> Redirection.redirect "/reference/"
      path "/reference/" >=> Files.browseFileHome "reference/index.html"
      browseHome
      >=> setHeader "Cache-Control" "public, max-age=3600"
      >=> setHeader "X-Content-Type-Options" "nosniff"
      >=> setHeader "X-Frame-Options" "SAMEORIGIN"
    ]

  let config =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP "0.0.0.0" 8080 ]
        homeFolder = Some contentDir }

module Main =
  [<EntryPoint>]
  let main argv =
    printfn "🚀 Starting Suave website on http://localhost:8080"
    printfn "Content directory: %s" Website.contentDir
    
    Web.startWebServer Website.config Website.app
    printfn "Press enter to exit..."
    System.Console.ReadLine() |> ignore
    0
