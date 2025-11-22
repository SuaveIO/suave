namespace Suave

open System
open System.Net.Sockets
open System.Threading
open System.Threading.Tasks
open System.Collections.Generic
open System.Collections.Concurrent
open Suave.Sockets
open Suave.Utils

/// Connection health status
type ConnectionHealth =
  | Healthy
  | Unhealthy of string

/// Represents an active connection being processed
type ActiveConnection<'T> = {
  connection: 'T
  acquiredAt: DateTime
}

/// Module for monitoring and managing connection health
module ConnectionHealthChecker =

  /// Tracks connections currently in use (not in pool)
  type ActiveConnectionTracker<'T>() =
    let activeConnections = ConcurrentDictionary<int, ActiveConnection<'T>>()
    let lockObj = obj()
    
    /// Register a connection as active (just popped from pool)
    member this.TrackConnection(connection: 'T) : unit =
      let id = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(connection)
      let now = DateTime.UtcNow
      activeConnections.TryAdd(id, { connection = connection; acquiredAt = now }) |> ignore
    
    /// Unregister a connection (returned to pool or closed)
    member this.UntrackConnection(connection: 'T) : unit =
      let id = System.Runtime.CompilerServices.RuntimeHelpers.GetHashCode(connection)
      activeConnections.TryRemove(id) |> ignore
    
    /// Get all currently active connections
    member this.GetActiveConnections() : ActiveConnection<'T> list =
      activeConnections.Values |> List.ofSeq
    
    /// Get connections that have exceeded max age (potential hangers)
    member this.GetStuckConnections(maxAgeSeconds: int) : ActiveConnection<'T> list =
      let now = DateTime.UtcNow
      let maxAge = TimeSpan.FromSeconds(float maxAgeSeconds)
      this.GetActiveConnections()
      |> List.filter (fun ac -> (now - ac.acquiredAt) > maxAge)
    
    /// Get count of active connections
    member this.ActiveConnectionCount : int =
      activeConnections.Count

  /// Attempts to detect if a socket is still healthy
  /// Uses a lightweight probe to detect error state
  let private probeSocketHealth (socket: Socket) : ConnectionHealth =
    try
      if socket = null then
        Unhealthy "Socket is null"
      elif socket.Connected = false then
        Unhealthy "Socket not connected"
      else
        // Check if socket is in error state without blocking
        if socket.Poll(0, SelectMode.SelectError) then
          Unhealthy "Socket in error state"
        else
          Healthy
    with
    | :? ObjectDisposedException -> Unhealthy "Socket disposed"
    | :? SocketException as ex -> Unhealthy (sprintf "Socket exception: %s" ex.Message)
    | ex -> Unhealthy (sprintf "Exception: %s" ex.Message)

  /// Configuration for health checker behavior
  type HealthCheckerConfig = {
    /// How often to run health checks (in milliseconds)
    checkIntervalMs: int
    /// Enable or disable health checking
    enabled: bool
    /// Whether to log diagnostics
    verbose: bool
    /// Maximum age for a connection to be active before considered stuck (seconds)
    maxConnectionAgeSeconds: int
  }

  /// Default health checker configuration
  let defaultConfig = {
    checkIntervalMs = 30000  // 30 seconds
    enabled = true
    verbose = false
    maxConnectionAgeSeconds = 300  // 5 minutes
  }

  /// Starts the background health checker for active connections
  let startHealthChecker<'T when 'T :> IDisposable> 
      (tracker: ActiveConnectionTracker<'T>)
      (config: HealthCheckerConfig)
      (getSocket: 'T -> Socket option)
      (isLongLived: 'T -> bool)
      (closeConnection: 'T -> unit) : Task =
    
    if not config.enabled then
      Task.CompletedTask
    else
      let cancellationTokenSource = new CancellationTokenSource()
      let mutable isRunning = true
      let mutable connectionsChecked = 0L
      let mutable connectionsForced = 0L

      let checkerLoop () = task {
        try
          while isRunning && not (cancellationTokenSource.Token.IsCancellationRequested) do
            try
              do! Task.Delay(config.checkIntervalMs, cancellationTokenSource.Token)
              
              // Get all active connections
              let activeConns = tracker.GetActiveConnections()
              Interlocked.Add(&connectionsChecked, int64 activeConns.Length) |> ignore
              
              if config.verbose then
                Console.WriteLine($"[HealthChecker] Monitoring {activeConns.Length} active connections")
              
              // Check each active connection for health
              for activeConn in activeConns do
                // Skip long-lived connections (WebSocket, SSE, etc.)
                if isLongLived activeConn.connection then
                  if config.verbose then
                    Console.WriteLine($"[HealthChecker] Skipping health check for long-lived connection (age {(DateTime.UtcNow - activeConn.acquiredAt).TotalSeconds:F1}s)")
                else
                  let health = 
                    match getSocket activeConn.connection with
                    | Some sock -> probeSocketHealth sock
                    | None -> Healthy
                  
                  let age = DateTime.UtcNow - activeConn.acquiredAt
                  let isExpired = age.TotalSeconds > float config.maxConnectionAgeSeconds
                  
                  match health with
                  | Unhealthy reason ->
                    if config.verbose then
                      Console.WriteLine($"[HealthChecker] Unhealthy connection detected (age {age.TotalSeconds:F1}s): {reason}")
                    try
                      closeConnection activeConn.connection
                      tracker.UntrackConnection activeConn.connection
                      Interlocked.Increment(&connectionsForced) |> ignore
                    with ex ->
                      if config.verbose then
                        Console.WriteLine($"[HealthChecker] Error closing unhealthy connection: {ex.Message}")
                  
                  | Healthy when isExpired ->
                    if config.verbose then
                      Console.WriteLine($"[HealthChecker] Connection exceeded max age ({age.TotalSeconds:F1}s > {config.maxConnectionAgeSeconds}s), forcing close")
                    try
                      closeConnection activeConn.connection
                      tracker.UntrackConnection activeConn.connection
                      Interlocked.Increment(&connectionsForced) |> ignore
                    with ex ->
                      if config.verbose then
                        Console.WriteLine($"[HealthChecker] Error closing aged connection: {ex.Message}")
                  
                  | Healthy -> ()
              
              if config.verbose && int64 activeConns.Length > 0L then
                Console.WriteLine($"[HealthChecker] Checked {activeConns.Length} connections, forced close on {Interlocked.Read(&connectionsForced)} stuck ones")
            with
            | :? OperationCanceledException -> ()
            | ex -> 
              if config.verbose then
                Console.WriteLine($"[HealthChecker] Error in health check loop: {ex.Message}")
        finally
          isRunning <- false
      }

      let task = checkerLoop ()
      task

  /// Stops the background health checker
  let stopHealthChecker (checkerTask: Task) : Task =
    checkerTask

  /// Checks health of a single connection
  let checkConnectionHealth (socket: Socket option) : ConnectionHealth =
    match socket with
    | Some sock -> probeSocketHealth sock
    | None -> Unhealthy "No socket available"
