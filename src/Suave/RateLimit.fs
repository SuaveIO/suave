namespace Suave

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Net
open System.Threading

/// Rate limiting module for Suave web server
module RateLimit =

  /// Strategy for rate limiting
  type RateLimitStrategy =
    /// Fixed window: Resets counter at fixed intervals
    | FixedWindow
    /// Sliding window: More accurate, considers requests in the last N seconds
    | SlidingWindow
    /// Token bucket: Allows bursts up to bucket capacity
    | TokenBucket of refillRate:float

  /// Configuration for rate limiting
  type RateLimitConfig =
    { /// Time window for rate limiting
      windowSize: TimeSpan
      /// Maximum number of requests allowed in the window
      maxRequests: int
      /// Function to extract key from context (e.g., IP address, user ID)
      keyExtractor: HttpContext -> string
      /// Rate limiting strategy to use
      strategy: RateLimitStrategy
      /// Message to return when rate limit is exceeded
      limitExceededMessage: string option
      /// Retry-After header value in seconds when limit exceeded
      retryAfter: int option }

  /// Internal state for fixed window strategy
  type private FixedWindowState =
    { mutable Count: int
      mutable WindowStart: DateTimeOffset
      Lock: obj }

  /// Internal state for sliding window strategy
  type private SlidingWindowState =
    { Timestamps: ConcurrentQueue<DateTimeOffset>
      Lock: obj }

  /// Internal state for token bucket strategy
  type private TokenBucketState =
    { mutable Tokens: float
      mutable LastRefill: DateTimeOffset
      Lock: obj }

  /// State container for rate limiting
  type private RateLimitState =
    | Fixed of FixedWindowState
    | Sliding of SlidingWindowState
    | Bucket of TokenBucketState

  /// Rate limiter storage
  type private RateLimiter(windowSize: TimeSpan) =
    let storage = ConcurrentDictionary<string, RateLimitState>()
    
    member _.Storage = storage

    member this.CheckFixedWindow(key: string, now: DateTimeOffset, config: RateLimitConfig) =
      let state =
        storage.GetOrAdd(key, fun _ -> 
          Fixed { Count = 0; WindowStart = now; Lock = obj() })

      match state with
      | Fixed fs ->
          lock fs.Lock (fun () ->
            // Check if window has expired
            if now - fs.WindowStart >= config.windowSize then
              fs.WindowStart <- now
              fs.Count <- 0
            
            // Increment count
            fs.Count <- fs.Count + 1
            
            // Check if within limit
            fs.Count <= config.maxRequests
          )
      | _ -> 
          false

    member this.CheckSlidingWindow(key: string, now: DateTimeOffset, config: RateLimitConfig) =
      let state =
        storage.GetOrAdd(key, fun _ -> 
          Sliding { Timestamps = ConcurrentQueue<DateTimeOffset>(); Lock = obj() })

      match state with
      | Sliding ss ->
          lock ss.Lock (fun () ->
            // Remove timestamps outside the window first
            let windowStart = now - config.windowSize
            let mutable oldTimestamp = DateTimeOffset.MinValue
            while ss.Timestamps.TryPeek(&oldTimestamp) && oldTimestamp < windowStart do
              ss.Timestamps.TryDequeue(&oldTimestamp) |> ignore
            
            // Check if we can add another request
            if ss.Timestamps.Count >= config.maxRequests then
              false  // Already at limit
            else
              // Add current timestamp and allow
              ss.Timestamps.Enqueue(now)
              true
          )
      | _ -> false

    member this.CheckTokenBucket(key: string, now: DateTimeOffset, config: RateLimitConfig, refillRate: float) =
      let state =
        storage.GetOrAdd(key, fun _ -> 
          Bucket { Tokens = float config.maxRequests; LastRefill = now; Lock = obj() })

      match state with
      | Bucket bs ->
          lock bs.Lock (fun () ->
            // Calculate tokens to add based on elapsed time
            let elapsed = (now - bs.LastRefill).TotalSeconds
            let tokensToAdd = elapsed * refillRate
            
            // Refill tokens (capped at maxRequests)
            let newTokens = min (bs.Tokens + tokensToAdd) (float config.maxRequests)
            bs.Tokens <- newTokens
            bs.LastRefill <- now
            
            // Check if we have at least 1 token before consuming
            if bs.Tokens >= 1.0 then
              bs.Tokens <- bs.Tokens - 1.0
              true
            else
              false
          )
      | _ -> false

    member this.Cleanup(maxAge: TimeSpan) =
      // Never drop state that is still inside its window
      let maxAge = if windowSize > maxAge then windowSize else maxAge
      let now = Globals.utcNow()
      let keysToRemove = ResizeArray<string>()
      
      for kvp in storage do
        let shouldRemove =
          match kvp.Value with
          | Fixed fs -> now - fs.WindowStart > maxAge
          | Sliding ss ->
              let mutable oldTimestamp = DateTimeOffset.MinValue
              ss.Timestamps.TryPeek(&oldTimestamp) |> ignore
              now - oldTimestamp > maxAge
          | Bucket bs -> now - bs.LastRefill > maxAge
        
        if shouldRemove then
          keysToRemove.Add(kvp.Key)
      
      for key in keysToRemove do
        storage.TryRemove(key) |> ignore
      
      keysToRemove.Count

  /// Every limiter created by `rateLimit`, held weakly so that the cleanup
  /// timer can visit them without keeping discarded limiters alive
  let private limiters = ResizeArray<WeakReference<RateLimiter>>()
  let private limitersLock = obj()

  /// Cleanup timer reference (ref cell for lazy initialization)
  let private cleanupTimer = ref None
  let private timerLock = obj()

  let private cleanupAll (maxAge: TimeSpan) =
    let live =
      lock limitersLock (fun () ->
        limiters.RemoveAll(fun wr -> match wr.TryGetTarget() with | true, _ -> false | _ -> true) |> ignore
        limiters |> Seq.choose (fun wr -> match wr.TryGetTarget() with | true, l -> Some l | _ -> None) |> List.ofSeq)
    for limiter in live do
      limiter.Cleanup(maxAge) |> ignore

  /// Start cleanup timer lazily on first use
  let private ensureCleanupTimer() =
    match cleanupTimer.Value with
    | Some _ -> () // Already started
    | None ->
        lock timerLock (fun () ->
          match cleanupTimer.Value with
          | Some _ -> () // Another thread started it
          | None ->
              let interval = TimeSpan.FromMinutes(5.0)
              let maxAge = TimeSpan.FromHours(1.0)
              let timer = new System.Threading.Timer(
                (fun _ -> cleanupAll maxAge),
                null,
                interval,
                interval)
              cleanupTimer := Some timer
        )

  let private registerLimiter (limiter: RateLimiter) =
    lock limitersLock (fun () -> limiters.Add(WeakReference<RateLimiter>(limiter)))

  let private normalizeIp (ip: IPAddress) =
    if ip.IsIPv4MappedToIPv6 then ip.MapToIPv4() else ip

  /// Default key extractor - uses the address of the connection's remote end.
  ///
  /// X-Forwarded-For and X-Real-IP are ignored: a client can set them to any
  /// value, so trusting them by default lets it pick a fresh key per request
  /// and never be limited. Behind a reverse proxy every request then comes from
  /// the proxy's address; use `trustedProxyKeyExtractor` in that case.
  let defaultKeyExtractor (ctx: HttpContext) : string =
    (normalizeIp (ctx.clientIp false [])).ToString()

  /// Key extractor for servers behind reverse proxies. When the connection
  /// comes from one of `trustedProxies`, the client address is taken from
  /// X-Forwarded-For: the right-most entry that is not itself a trusted proxy
  /// (entries to its left were supplied by the client and are ignored). Without
  /// X-Forwarded-For, X-Real-IP is used. Connections from any other address are
  /// keyed by that address, as in `defaultKeyExtractor`, so forwarding headers
  /// sent directly by clients have no effect.
  let trustedProxyKeyExtractor (trustedProxies: IPAddress list) : HttpContext -> string =
    let trusted = HashSet<IPAddress>(trustedProxies |> Seq.map normalizeIp)
    let tryParse (value: string) =
      match IPAddress.TryParse(value.Trim()) with
      | true, ip -> Some (normalizeIp ip)
      | false, _ -> None
    fun (ctx: HttpContext) ->
      let remote = normalizeIp (ctx.clientIp false [])
      if not (trusted.Contains remote) then
        remote.ToString()
      else
        let forwarded =
          match Headers.getAll ctx.request.headers "x-forwarded-for" with
          | Choice1Of2 (values: string seq) ->
              values
              |> List.ofSeq
              |> List.collect (fun (v: string) -> v.Split(',') |> List.ofArray)
              |> List.map tryParse
          | Choice2Of2 _ -> []
        let fromForwarded =
          forwarded
          |> List.rev
          |> List.tryPick (function
            | Some (ip: IPAddress) when not (trusted.Contains ip) -> Some ip
            | _ -> None)
        match fromForwarded with
        | Some ip -> ip.ToString()
        | None when not (List.isEmpty forwarded) ->
            // Every entry was a trusted proxy (or unparseable)
            remote.ToString()
        | None ->
            match ctx.request.header "x-real-ip" with
            | Choice1Of2 value ->
                match tryParse value with
                | Some ip -> ip.ToString()
                | None -> remote.ToString()
            | Choice2Of2 _ -> remote.ToString()

  /// Key extractor based on user identity
  let userKeyExtractor (ctx: HttpContext) : string =
    match ctx.userState.TryGetValue("userName") with
    | true, userName -> userName :?> string
    | false, _ -> defaultKeyExtractor ctx

  /// Key extractor based on custom header. The header is supplied by the
  /// client, so only use this for values the client cannot choose freely
  /// (e.g. an API key that is validated before or by the protected WebPart).
  let headerKeyExtractor (headerName: string) (ctx: HttpContext) : string =
    match ctx.request.header headerName with
    | Choice1Of2 value -> value
    | Choice2Of2 _ -> defaultKeyExtractor ctx

  /// Default rate limit configuration: 60 requests per minute per client
  /// address, fixed window
  let defaultRateLimitConfig =
    { windowSize = TimeSpan.FromMinutes(1.0)
      maxRequests = 60  // 60 requests per minute
      keyExtractor = defaultKeyExtractor
      strategy = FixedWindow
      limitExceededMessage = None
      retryAfter = None }

  /// Default rate limit configuration
  [<Obsolete("Use RateLimit.defaultRateLimitConfig. After `open Suave.RateLimit` this value shadows the server's `defaultConfig`.")>]
  let defaultConfig = defaultRateLimitConfig

  /// Check if request is allowed under rate limit
  let private checkRateLimit (limiter: RateLimiter) (config: RateLimitConfig) (ctx: HttpContext) : bool =
    // Ensure cleanup timer is started on first use
    ensureCleanupTimer()
    
    let key = config.keyExtractor ctx
    let now = Globals.utcNow()

    match config.strategy with
    | FixedWindow ->
        limiter.CheckFixedWindow(key, now, config)
    | SlidingWindow ->
        limiter.CheckSlidingWindow(key, now, config)
    | TokenBucket refillRate ->
        limiter.CheckTokenBucket(key, now, config, refillRate)

  /// Rate limiting WebPart that wraps another WebPart. Each call keeps its own
  /// counters, so two `rateLimit` WebParts never share a limit even when their
  /// keys are equal.
  let rateLimit (config: RateLimitConfig) (protectedPart: WebPart) : WebPart =
    let limiter = RateLimiter(config.windowSize)
    registerLimiter limiter
    fun (ctx: HttpContext) ->
      async {
        let allowed = checkRateLimit limiter config ctx
        
        if allowed then
          // Request allowed - call the protected WebPart
          return! protectedPart ctx
        else
          // Rate limit exceeded - write 429 response
          let message = 
            config.limitExceededMessage 
            |> Option.defaultValue "Rate limit exceeded. Please try again later."
          
          // Set headers and write response
          let! ctx1 =
            match config.retryAfter with
            | Some seconds -> Writers.setHeader "Retry-After" (string seconds) ctx
            | None -> async.Return (Some ctx)
          
          match ctx1 with
          | Some c1 ->
              match! Writers.setHeader "X-RateLimit-Limit" (string config.maxRequests) c1 with
              | Some c2 ->
                  // Write the 429 response
                  return! RequestErrors.TOO_MANY_REQUESTS message c2
              | None ->
                  return None
          | None ->
              return None
      }

  /// Create a rate limiter with requests per second
  let perSecond (requests: int) =
    { defaultRateLimitConfig with
        windowSize = TimeSpan.FromSeconds(1.0)
        maxRequests = requests }

  /// Create a rate limiter with requests per minute
  let perMinute (requests: int) =
    { defaultRateLimitConfig with
        windowSize = TimeSpan.FromMinutes(1.0)
        maxRequests = requests }

  /// Create a rate limiter with requests per hour
  let perHour (requests: int) =
    { defaultRateLimitConfig with
        windowSize = TimeSpan.FromHours(1.0)
        maxRequests = requests }

  /// Set the key extractor function
  let withKeyExtractor (extractor: HttpContext -> string) (config: RateLimitConfig) =
    { config with keyExtractor = extractor }

  /// Set the rate limiting strategy
  let withStrategy (strategy: RateLimitStrategy) (config: RateLimitConfig) =
    { config with strategy = strategy }

  /// Set the message when rate limit is exceeded
  let withMessage (message: string) (config: RateLimitConfig) =
    { config with limitExceededMessage = Some message }

  /// Set the Retry-After header value
  let withRetryAfter (seconds: int) (config: RateLimitConfig) =
    { config with retryAfter = Some seconds }

  /// Get current rate limit statistics for a key. Every `rateLimit` WebPart
  /// keeps its own counters; this returns the statistics of the first live
  /// limiter that has an entry for the key.
  let getStats (keyExtractor: HttpContext -> string) (ctx: HttpContext) : Map<string, obj> option =
    let key = keyExtractor ctx
    let found =
      lock limitersLock (fun () ->
        limiters
        |> Seq.tryPick (fun wr ->
          match wr.TryGetTarget() with
          | true, limiter ->
              match limiter.Storage.TryGetValue(key) with
              | true, state -> Some state
              | false, _ -> None
          | _ -> None))
    match found with
    | Some state ->
        let stats =
          match state with
          | Fixed fs ->
              Map.ofList [
                "strategy", box "FixedWindow"
                "count", box fs.Count
                "windowStart", box fs.WindowStart
              ]
          | Sliding ss ->
              Map.ofList [
                "strategy", box "SlidingWindow"
                "count", box ss.Timestamps.Count
              ]
          | Bucket bs ->
              Map.ofList [
                "strategy", box "TokenBucket"
                "tokens", box bs.Tokens
                "lastRefill", box bs.LastRefill
              ]
        Some stats
    | None -> None
