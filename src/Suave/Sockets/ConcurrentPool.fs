namespace Suave.Sockets

open System
open System.Collections.Concurrent
open System.Threading
open System.Threading.Tasks

type ConcurrentPool<'T>() =

  let objects = ConcurrentBag<'T>()
  let mutable objectGenerator: Func<'T> = null
  let mutable maxSize = Int32.MaxValue
  let mutable currentSize = 0
  
  // Metrics for monitoring pool behavior
  let mutable totalCreated = 0L
  let mutable totalReused = 0L
  let mutable totalReturned = 0L
  
  // Health checker configuration and state
  let mutable healthCheckEnabled = false
  let mutable healthCheckIntervalMs = 30000
  let mutable healthCheckTask: Task option = None
  
  // Active connection tracking for health checking
  let mutable onConnectionAcquired: ('T -> unit) option = None
  let mutable onConnectionReturned: ('T -> unit) option = None
  let mutable onConnectionReset: ('T -> unit) option = None  // Reset state before reuse

  member x.Pop() =
    let item = 
      match objects.TryTake() with
      | true, item ->
        Interlocked.Increment(&totalReused) |> ignore
        item
      | _, _ -> 
        Interlocked.Increment(&currentSize) |> ignore
        Interlocked.Increment(&totalCreated) |> ignore
        objectGenerator.Invoke()
    
    // Reset connection state (e.g., clear isLongLived flag)
    match onConnectionReset with
    | Some callback -> callback item
    | None -> ()
    
    // Notify tracker that connection is now active
    match onConnectionAcquired with
    | Some callback -> callback item
    | None -> ()
    
    item

  member x.Push(item) =
    // Notify tracker that connection is being returned to pool
    match onConnectionReturned with
    | Some callback -> callback item
    | None -> ()
    
    // Only add back to pool if under size limit
    if currentSize <= maxSize then
      objects.Add(item)
      Interlocked.Increment(&totalReturned) |> ignore
    else
      // Dispose if item is IDisposable and we're over limit
      match box item with
      | :? IDisposable as disposable -> disposable.Dispose()
      | _ -> ()
      Interlocked.Decrement(&currentSize) |> ignore

  member this.ObjectGenerator
    with get () = objectGenerator
    and set (value) = objectGenerator <- value
  
  member this.MaxSize
    with get () = maxSize
    and set (value) = maxSize <- value
  
  // Health check configuration properties
  member this.HealthCheckEnabled
    with get () = healthCheckEnabled
    and set (value) = healthCheckEnabled <- value
  
  member this.HealthCheckIntervalMs
    with get () = healthCheckIntervalMs
    and set (value) = healthCheckIntervalMs <- value
  
  member this.HealthCheckTask
    with get () = healthCheckTask
    and set (value) = healthCheckTask <- value
  
  // Active connection tracking callbacks
  member this.OnConnectionAcquired
    with get () = onConnectionAcquired
    and set (value) = onConnectionAcquired <- value
  
  member this.OnConnectionReturned
    with get () = onConnectionReturned
    and set (value) = onConnectionReturned <- value
  
  member this.OnConnectionReset
    with get () = onConnectionReset
    and set (value) = onConnectionReset <- value

  // Metrics properties
  member this.CurrentSize = currentSize
  member this.TotalCreated = totalCreated
  member this.TotalReused = totalReused
  member this.TotalReturned = totalReturned
  member this.ReuseRate = 
    if totalCreated + totalReused > 0L then
      float totalReused / float (totalCreated + totalReused)
    else
      0.0

  /// Iterate over all objects currently in the pool
  /// Each object is temporarily removed during iteration and returned afterward
  member this.Iterate(action: 'T -> unit) =
    let items = ResizeArray<'T>()
    // Extract all items
    let mutable item = Unchecked.defaultof<'T>
    while objects.TryTake(&item) do
      items.Add(item)
    
    try
      // Process each item
      for item in items do
        action(item)
    finally
      // Return all items to pool
      for item in items do
        objects.Add(item)

  /// Try to remove an unhealthy item from the pool permanently
  /// Returns true if item was found and removed
  member this.TryRemove(predicate: 'T -> bool) : int =
    let items = ResizeArray<'T>()
    let mutable removed = 0
    
    // Extract all items
    let mutable item = Unchecked.defaultof<'T>
    while objects.TryTake(&item) do
      if predicate item then
        removed <- removed + 1
        Interlocked.Decrement(&currentSize) |> ignore
        // Dispose if it's IDisposable
        match box item with
        | :? IDisposable as disposable -> disposable.Dispose()
        | _ -> ()
      else
        items.Add(item)
    
    // Return non-removed items to pool
    for item in items do
      objects.Add(item)
    
    removed
