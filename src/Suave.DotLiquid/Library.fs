/// A module for rendering DotLiquid template with Suave
module Suave.DotLiquid

open System
open System.Reflection
open System.Collections.Generic
open System.Collections.Concurrent
open System.IO
open DotLiquid
open DotLiquid.NamingConventions
open Microsoft.FSharp.Reflection
open Suave
open Suave.Utils
open Suave.Successful
open Suave.Files

// -------------------------------------------------------------------------------------------------
// Registering things with DotLiquid
// -------------------------------------------------------------------------------------------------

/// Use the ruby naming convention by default
do Template.NamingConvention <- RubyNamingConvention()

module internal Impl =

  /// Represents a local file system relative to the specified 'root'
  let localFileSystem root =
    { new DotLiquid.FileSystems.IFileSystem with
        member this.ReadTemplateFile(context, templateName) =
          let templatePath = context.[templateName] :?> string
          let fullPath = Path.Combine(root, templatePath)
          if not (File.Exists(fullPath)) then failwithf "File not found: %s" fullPath
          File.ReadAllText(fullPath) }

  /// Protects accesses to various DotLiquid internal things
  let safe =
    let o = obj()
    fun f -> lock o f

  open System.Reflection

  /// Given a type which is an F# record containing seq<_>, list<_>, array<_>, option and 
  /// other records, register the type with DotLiquid so that its fields are accessible
  let tryRegisterTypeTree =
    let registered = Dictionary<_, _>()
    let rec loop ty =
      if not (registered.ContainsKey ty) then
        if FSharpType.IsRecord ty then
          let fields = FSharpType.GetRecordFields ty
          Template.RegisterSafeType(ty, [| for f in fields -> f.Name |])
          for f in fields do loop f.PropertyType
        elif ty.IsGenericType then
          let t = ty.GetGenericTypeDefinition()
          if t = typedefof<seq<_>> || t = typedefof<list<_>>  then
            loop (ty.GetGenericArguments().[0])          
          elif t = typedefof<option<_>> then
            Template.RegisterSafeType(ty, [|"Value"|])
            loop (ty.GetGenericArguments().[0])            
        elif ty.IsArray then          
          loop (ty.GetElementType())
        registered.[ty] <- true
    fun ty -> safe (fun () -> loop ty)

  // -------------------------------------------------------------------------------------------------
  // Parsing and loading DotLiquid templates and caching the results
  // -------------------------------------------------------------------------------------------------

  /// Memoize asynchronous function. An item is recomputed when `isValid` returns `false`
  let asyncMemoize isValid f =
    let cache = ConcurrentDictionary<_ , _>()
    fun x -> async {
      match cache.TryGetValue x with
      | true, res when isValid x res ->
        return res

      | _ ->
        let! res = f x
        cache.[x] <- res
        return res
    }

  type Renderer<'model> = string -> 'model -> string

  /// Parse the specified template & register the type that we want to use as "model"
  let parseTemplate template typ : Renderer<'m> =
    tryRegisterTypeTree typ
    let t = Template.Parse template
    fun k v ->
      dict [k, box v] |> Hash.FromDictionary |> t.Render

  /// Asynchronously loads a template & remembers the last write time
  /// (so that we can automatically reload the template when file changes)
  let fileTemplate (typ, fileName) = async {
    let writeTime = File.GetLastWriteTime fileName
    use file = new FileStream(fileName, FileMode.Open, FileAccess.Read, FileShare.ReadWrite)
    use reader = new StreamReader(file)
    let! dotLiquidTemplate = reader.ReadToEndAsync()
    return writeTime, parseTemplate dotLiquidTemplate typ
  }

  /// Load template & memoize & automatically reload when the file changes
  let fileTemplateMemoized =
    fileTemplate
    |> asyncMemoize (fun (_, templatePath) (lastWrite, _) ->
      File.GetLastWriteTime templatePath <= lastWrite)

  let stringTemplate (typ, stringTemplate) =
    async.Return ((), parseTemplate stringTemplate typ)

  let stringTemplateMemoized =
    stringTemplate |> asyncMemoize (fun _ _ -> (* always valid *) true)

// -------------------------------------------------------------------------------------------------
// Public API
// -------------------------------------------------------------------------------------------------

let mutable private templatesDir = None

/// The model you pass to your liquid templates will be available under this
/// key.
[<Literal>]
let ModelKey = "model"

/// Sets the DotLiquid naming convention to the Ruby style. F# values in camelCase
/// will be converted to not_camel_case in templates. This is the default
let setRubyNamingConvention _ =
  Template.NamingConvention <- RubyNamingConvention()

/// Sets the DotLiquid naming convetion to the CSharp style, will preserve camelCase
/// and should preserve whatever case convention you use
let setCSharpNamingConvention _ =
  Template.NamingConvention <- CSharpNamingConvention()

/// Set the root directory where DotLiquid is looking for templates. For example, you can
/// write something like this:
///
///     DotLiquid.setTemplatesDir (__SOURCE_DIRECTORY__ + "/templates")
///
/// The current directory is a global variable and so it should not change
/// between multiple HTTP requests. This is a DotLiquid limitation.
let setTemplatesDir dir =
  if templatesDir <> Some dir then
    templatesDir <- Some dir
    Impl.safe (fun () -> Template.FileSystem <- Impl.localFileSystem dir)

/// Renders the liquid template given.
let renderPageString (template : string) (model : 'm) =
  Impl.stringTemplateMemoized (typeof<'m>, template)
  |> Async.map (fun (_, renderer) ->
    renderer ModelKey (box model))

/// Renders the liquid template given a full path.
let renderPageFile fileFullPath (model : 'm) =
  Impl.fileTemplateMemoized (typeof<'m>, fileFullPath)
  |> Async.map (fun (writeTime, renderer) ->
    renderer ModelKey (box model))

/// Render a page using DotLiquid template. Takes a path (relative to the directory specified
/// using `setTemplatesDir` and a value that is exposed as the "model" variable. You can use
/// any F# record type, seq<_>, list<_>, and array<_>  and option without having to explicitly 
/// register the fields.
///
///     type Page = { Total : int }
///     let app = page "index.html" { Total = 42 }
///
let page fileName model : WebPart =
  fun ctx ->
    let fullPath =
      match templatesDir with
      | None ->
        resolvePath ctx.runtime.homeDirectory fileName

      | Some root ->
        Path.Combine(root, fileName)

    async {
      let! rendered = renderPageFile fullPath model
      return! OK rendered ctx
    }

/// Register functions from a module as filters available in DotLiquid templates.
/// For example, the following snippet lets you write `{{ model.Total | nice_num }}`:
///
///     module MyFilters =
///       let niceNum i = if i > 10 then "lot" else "not much"
///
///     do registerFiltersByName "MyFilters"
///
let registerFiltersByName name =
  let asm = Assembly.GetEntryAssembly()
  let typ = 
    asm.GetTypes()
    |> Array.find (fun t -> t.FullName.EndsWith(name) && 
                            not(t.FullName.Contains("<StartupCode")))
  Template.RegisterFilter typ

  

/// Similar to `registerFiltersByName`, but the module is speicfied by its
/// `System.Type` (This is more cumbersome, but safer alternative.)
let registerFiltersByType typ =
  Template.RegisterFilter typ