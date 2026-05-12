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

/// Cached UTF8 encoding to avoid allocating new instances
let UTF8 = System.Text.Encoding.UTF8

/// Dedicated byte ArrayPool isolated from System.Buffers.ArrayPool.Shared.
///
/// Rationale: ArrayPool.Shared is process-wide and contended by every library
/// running in the host (the BCL itself, ASP.NET if hosted side-by-side,
/// JSON serialisers, gzip streams, …). Under high concurrency, even though
/// Shared has thread-local caches, the cross-bucket fall-through paths take
/// locks that show up in profiles. By renting Suave's per-connection buffers
/// (line buffer, header read buffer, header-name lowercase scratch) from a
/// pool we own, we keep our hot-path rentals out of that shared contention
/// surface entirely.
///
/// Tuning: ArrayPool.Create defaults are 1MB max length, 50 arrays per bucket.
/// Suave's typical rentals are 8KiB and 256B; the defaults are appropriate.
module BufferPool =
  open System.Buffers
  let pool : ArrayPool<byte> = ArrayPool<byte>.Create()

  /// Rent a byte array of at least the requested size from Suave's private pool.
  let inline rent (size: int) : byte[] = pool.Rent size

  /// Return a byte array to Suave's private pool.
  /// `clearArray = true` zeroes the contents to prevent any data bleed between connections.
  let inline returnArray (buf: byte[]) (clearArray: bool) = pool.Return(buf, clearArray)

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
