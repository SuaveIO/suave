open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.IO

let compressedFilesMap = new ConcurrentDictionary<struct(string * string),string * DateTime>()

let mutable MAX_COMPRESSED_FILE_AGE = TimeSpan.FromDays 1.
let mutable MAX_COMPRESSED_FILES = 1000

let private pendingDeletions = ConcurrentQueue<string>()
let private MAX_PENDING_DELETIONS = 10000

let internal tryDelete (path : string) =
  try
    File.Delete path
    true
  with
  | :? FileNotFoundException | :? DirectoryNotFoundException ->
    true
  | _ ->
    if pendingDeletions.Count < MAX_PENDING_DELETIONS then
      pendingDeletions.Enqueue path
    false

let internal retryPendingDeletions () =
  let mutable remaining = pendingDeletions.Count
  let mutable deleted = 0
  let mutable path = Unchecked.defaultof<string>
  while remaining > 0 && pendingDeletions.TryDequeue(&path) do
    remaining <- remaining - 1
    if tryDelete path then deleted <- deleted + 1
  deleted

let rec internal swapCacheEntry (k : struct (string * string)) (entry : string * DateTime) =
  let map = compressedFilesMap
  match map.TryGetValue k with
  | true, existing ->
    if map.TryUpdate(k, entry, existing) then Some (fst existing)
    else swapCacheEntry k entry
  | _ ->
    if map.TryAdd(k, entry) then None
    else swapCacheEntry k entry

let private openForRead (path : string) : Stream =
  new FileStream(path, FileMode.Open, FileAccess.Read, FileShare.Read ||| FileShare.Delete) :> Stream

let cleanupFolder (compressionFolder : string) (maxAge : TimeSpan) (maxFiles : int) : int =
  let mutable deleted = retryPendingDeletions ()
  try
    if Directory.Exists compressionFolder then
      let files =
        DirectoryInfo(compressionFolder).GetFiles()
        |> Array.sortBy (fun fi -> fi.LastWriteTimeUtc)
      let keysByPath = Dictionary<string, struct (string * string)>(StringComparer.Ordinal)
      for KeyValue(k, (path, _)) in compressedFilesMap do
        keysByPath.[path] <- k
      let now = DateTime.UtcNow
      let overBound = files.Length - (max 0 maxFiles)
      for i in 0 .. files.Length - 1 do
        let fi = files.[i]
        if i < overBound || now - fi.LastWriteTimeUtc > maxAge then
          match keysByPath.TryGetValue fi.FullName with
          | true, k -> compressedFilesMap.TryRemove k |> ignore
          | _ -> ()
          if tryDelete fi.FullName then deleted <- deleted + 1
  with _ -> ()
  deleted

let cacheKey (key : string) (n : string) : struct (string * string) = struct (key, n)

let tryFindExisting (key:string) (n:string) (lastModified:DateTime) =
  let map = compressedFilesMap
  let k = cacheKey key n
  match map.TryGetValue k with
  | true, ((existingPath, prevLastModified) as existing) when lastModified <= prevLastModified ->
      if File.Exists existingPath then
        Some existingPath
      else
        let item = KeyValuePair<struct (string * string), string * DateTime>(k, existing)
        map.TryRemove item |> ignore
        None
  | _ ->
      None

let tryOpenExisting (path : string) =
  try Some (openForRead path)
  with
  | :? IOException | :? UnauthorizedAccessException -> None

// exercise the code paths
let dir = Path.Combine(Path.GetTempPath(), "suave-check-" + Guid.NewGuid().ToString("N"))
Directory.CreateDirectory dir |> ignore
let older = Path.Combine(dir, "old")
let recent = Path.Combine(dir, "recent")
File.WriteAllText(older, "old")
File.SetLastWriteTime(older, DateTime.Now - TimeSpan.FromHours 2.)
File.WriteAllText(recent, "recent")
compressedFilesMap.[cacheKey older "gzip"] <- (older, DateTime.Now)
printfn "deleted=%d" (cleanupFolder dir (TimeSpan.FromHours 1.) 1000)
printfn "old exists=%b recent exists=%b map=%d" (File.Exists older) (File.Exists recent) compressedFilesMap.Count
printfn "swap1=%A" (swapCacheEntry (cacheKey "k" "gzip") ("p1", DateTime.Now))
printfn "swap2=%A" (swapCacheEntry (cacheKey "k" "gzip") ("p2", DateTime.Now))
printfn "find=%A" (tryFindExisting "k" "gzip" (DateTime.Now - TimeSpan.FromDays 1.))
printfn "openMissing=%b" (tryOpenExisting (Path.Combine(dir, "nope")) |> Option.isSome)
Directory.Delete(dir, true)
