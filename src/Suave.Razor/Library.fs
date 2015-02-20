namespace Suave

open System
open System.IO
open Suave.Types
open Suave.Types.Codes
open Suave.Http
open Suave.Http.Files
open Suave.Utils

open RazorEngine

module Razor =

  let private async_memoize f =
    let cache = Collections.Concurrent.ConcurrentDictionary<_ , _>()
    fun x ->
      async {
        let mutable res = Unchecked.defaultof<_>
        let ok = cache.TryGetValue(x,&res)
        if ok then 
          return res
        else 
          let! res = f x
          cache.[x] <- res
          return res
      }

  let private load_template template_path =
    async {
      use file = new FileStream(template_path, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
      use reader = new StreamReader(file)
      let! razorTemplate = reader.ReadToEndAsync()
      return razorTemplate
    }

  /// razor WebPart
  ///
  /// type Bar = { foo : string }
  ///
  /// let app : WebPart =
  ///   url "/home" >>= razor "/home.chtml" { foo = "Bar" }
  ///
  let razor<'a> path (model : 'a) =

    let load_template = async_memoize load_template

    fun r ->
      async {
        try
          let template_path = resolvePath r.runtime.homeDirectory path
          let! razorTemplate = load_template template_path
          let content = Razor.Parse(razorTemplate, model, template_path)
          return! Response.response HTTP_200 (UTF8.bytes content) r
        with 
          ex ->
          return! Response.response HTTP_500 (UTF8.bytes (ex.ToString())) r
        }