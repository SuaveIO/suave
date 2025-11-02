module Suave.Globals

open System

/// Get the current DateTimeOffset in UTC format.
///
/// If you are unit-testing
/// you can set this, but as with all globals, you need to set it back afterwards
/// or you will break subsequent unit tests.
let mutable utcNow = fun () -> System.DateTimeOffset.UtcNow

/// Cached HTTP date string to avoid formatting on every request.
/// HTTP dates only change once per second, so caching provides significant benefit.
module DateCache =
  let mutable private cachedDateString = ""
  let mutable private cachedDateBytes: byte[] = [||]
  let mutable private lastSecond = 0L
  let private lockObj = obj()
  
  /// Get the current HTTP date as a formatted string.
  /// Caches the result and only updates once per second.
  let getHttpDateString() =
    let now = utcNow()
    let currentSecond = now.ToUnixTimeSeconds()
    
    if currentSecond <> lastSecond then
      lock lockObj (fun () ->
        if currentSecond <> lastSecond then
          cachedDateString <- now.ToString("R")
          cachedDateBytes <- System.Text.Encoding.ASCII.GetBytes(cachedDateString)
          lastSecond <- currentSecond
      )
    
    cachedDateString
  
  /// Get the current HTTP date as ASCII bytes.
  /// Caches the result and only updates once per second.
  let getHttpDateBytes() =
    let now = utcNow()
    let currentSecond = now.ToUnixTimeSeconds()
    
    if currentSecond <> lastSecond then
      lock lockObj (fun () ->
        if currentSecond <> lastSecond then
          cachedDateString <- now.ToString("R")
          cachedDateBytes <- System.Text.Encoding.ASCII.GetBytes(cachedDateString)
          lastSecond <- currentSecond
      )
    
    cachedDateBytes

/// From the TCP module, keeps track of the number of clients
let internal numberOfClients = ref 0L

/// StringBuilder pool for dynamic content generation
module StringBuilderPool =
  open System.Text
  open System.Collections.Concurrent
  open System.Threading
  
  let private pool = new ConcurrentBag<StringBuilder>()
  let private maxPoolSize = 50
  let private poolSize = ref 0
  let private defaultCapacity = 256
  
  /// Get a StringBuilder from the pool or create a new one
  let Get() =
    match pool.TryTake() with
    | true, sb ->
      Interlocked.Decrement(poolSize) |> ignore
      sb
    | _, _ ->
      new StringBuilder(defaultCapacity)
  
  /// Return a StringBuilder to the pool after clearing it
  let Return(sb: StringBuilder) =
    if !poolSize < maxPoolSize then
      sb.Clear() |> ignore
      // Only keep reasonably sized StringBuilders
      if sb.Capacity <= 4096 then
        pool.Add(sb)
        Interlocked.Increment(poolSize) |> ignore
      // If too large, let it be garbage collected

/// Dictionary pool for HttpContext userState
module DictionaryPool =
  open System.Collections.Generic
  open System.Collections.Concurrent
  open System.Threading
  
  let private pool = new ConcurrentBag<Dictionary<string, obj>>()
  let private maxPoolSize = 100
  let private poolSize = ref 0
  
  /// Get a Dictionary from the pool or create a new one
  let Get() =
    match pool.TryTake() with
    | true, dict ->
      Interlocked.Decrement(poolSize) |> ignore
      dict
    | _, _ ->
      new Dictionary<string, obj>()
  
  /// Return a Dictionary to the pool after clearing it
  let Return(dict: Dictionary<string, obj>) =
    if !poolSize < maxPoolSize then
      dict.Clear()
      // Only keep reasonably sized dictionaries
      if dict.Count = 0 then  // Ensure it's cleared
        pool.Add(dict)
        Interlocked.Increment(poolSize) |> ignore
      // If something went wrong with clearing, let it be garbage collected

open System.Collections.Concurrent

/// A map of compressed files. The key is the is the name of the file, and value is 
/// a pair: the name of the compressed file and timestamp of the original file at 
/// time of compression.
let internal compressedFilesMap = new ConcurrentDictionary<string,string * DateTime>()

open System.Reflection

/// This returns the assembly version of Suave
let SuaveVersion = Assembly.GetExecutingAssembly().GetName().Version.ToString()

/// This is the server header
let ServerHeader = "Server: Suave (https://suave.io)"

let mutable verbose = false

open System.Diagnostics

[<AbstractClass; Sealed>]
type Logger =

    [<Conditional("DEBUG")>]
    static member debug (s:string) : unit =
      Console.WriteLine s

    static member info (s:string) : unit =
      Console.WriteLine s


[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.LibUv")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Experimental")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.DotLiquid")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Razor")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Xsp")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Tests")>]

do ()
