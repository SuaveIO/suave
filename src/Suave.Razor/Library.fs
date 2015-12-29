namespace Suave

open System
open System.IO
open Suave.Http
open Suave.Files
open Suave.Utils
open Suave.Utils.AsyncExtensions

open RazorEngine

module Razor =
  let private loadTemplate templatePath =
    async {
      let writeTime = File.GetLastWriteTime(templatePath)
      use file = new FileStream(templatePath, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
      use reader = new StreamReader(file)
      let! razorTemplate = reader.ReadToEndAsync()
      return writeTime, razorTemplate
    }

  open RazorEngine.Configuration
  open RazorEngine.Templating

  let cachingProvider = new InvalidatingCachingProvider(fun t -> ())
  let internal writeTimeCache = new System.Collections.Concurrent.ConcurrentDictionary<string, DateTime>()
  let isTemplateUpdated templatePath =
    let lastWriteTime = File.GetLastWriteTime templatePath
    let mutable wasUpdated = false
    writeTimeCache.AddOrUpdate(templatePath, lastWriteTime, fun key oldDate ->
      wasUpdated <- lastWriteTime > oldDate
      lastWriteTime) |> ignore
    wasUpdated
  let private (@@) a b = Path.Combine(a, b)

  let resolveView r templatePath =
   resolvePath (r.runtime.homeDirectory @@ "Views") templatePath
   |> Path.GetFullPath


  /// razor WebPart
  ///
  /// type Bar = { foo : string }
  ///
  /// let app : WebPart =
  ///   url "/home" >>= razor "home" { foo = "Bar" }
  ///
  let razor<'a> (path:string) (model : 'a) =
    fun r ->
      async {
        let languageOptions =
          Headers.getHeader "Accept-Language" r
          |> Headers.getSplitValues
          |> Headers.getWeightedValues
          |> Seq.choose (fst >> Headers.parseCultureInfo)

        let resolvePaths =
          (r.request.url.Segments
           |> Seq.fold (fun paths item ->
             match paths with
             | h :: _ -> (h @@ item) :: paths
             | _ -> [ item ]) []
           |> Seq.map (resolveView r)
           |> Seq.toList)
          @ [ resolvePath r.runtime.homeDirectory "." |> Path.GetFullPath ]

        let serviceConfiguration = TemplateServiceConfiguration()
        // generate compiled templates in memory instead of on disk as temporary files
        serviceConfiguration.DisableTempFileLocking <- true
        serviceConfiguration.CachingProvider <- cachingProvider
        serviceConfiguration.TemplateManager <-
          { new ITemplateManager with
              member x.AddDynamic(_, _) = failwith "not implemented"
              member x.Resolve key =
                let _, readTemplate = loadTemplate key.Name |> Async.RunSynchronously
                LoadedTemplateSource(readTemplate, key.Name) :> _
              member x.GetKey (name, resolveType, context) =
                let resolvedEntry =
                  if not <| Path.IsPathRooted name then
                    let pathWithoutExtension =
                      Path.GetDirectoryName name @@ Path.GetFileNameWithoutExtension name

                    let pathsWithExtension =
                      languageOptions
                      |> Seq.map (fun ci -> sprintf "%s-%s.cshtml" pathWithoutExtension ci.IetfLanguageTag)
                      |> Seq.append [ sprintf "%s.cshtml" pathWithoutExtension ]

                    resolvePaths
                    |> Seq.collect (fun path ->
                      pathsWithExtension
                      |> Seq.map (fun pathWithExtension -> path @@ pathWithExtension))
                    |> Seq.filter File.Exists
                    |> Seq.tryFind (fun _ -> true)
                    // |> Seq.tryHead // F# 4
                  else Some name
                match resolvedEntry with
                | Some (resolvedPath) ->
                  let key = NameOnlyTemplateKey(resolvedPath, resolveType, context) :> ITemplateKey
                  if isTemplateUpdated resolvedPath then
                    cachingProvider.InvalidateCache(key)
                  key
                | _ -> 
                  failwithf "Could not resolve requested view '%s' in '%A'" name resolvePaths
                 }

        let razorService = RazorEngineService.Create(serviceConfiguration)
        let viewBag = RazorEngine.Templating.DynamicViewBag()
        viewBag.AddValue("HttpContext", r)
        let content = razorService.RunCompile(path, typeof<'a>, model, viewBag)
        return! Response.response HTTP_200 (System.Text.Encoding.UTF8.GetBytes content) r
      }