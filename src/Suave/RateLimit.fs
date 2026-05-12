namespace Suave

open System
open System.Collections.Concurrent
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
  type private RateLimiter() =
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

  /// Global rate limiter instance
  let private globalLimiter = RateLimiter()

  /// Cleanup timer reference (ref cell for lazy initialization)
  let private cleanupTimer = ref None
  let private timerLock = obj()

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
                (fun _ -> globalLimiter.Cleanup(maxAge) |> ignore),
                null,
                interval,
                interval)
              cleanupTimer := Some timer
        )

  /// Default key extractor - uses client IP address
  let defaultKeyExtractor (ctx: HttpContext) : string =
    ctx.clientIpTrustProxy.ToString()

  /// Key extractor based on user identity
  let userKeyExtractor (ctx: HttpContext) : string =
    match ctx.userState.TryGetValue("userName") with
    | true, userName -> userName :?> string
    | false, _ -> defaultKeyExtractor ctx

  /// Key extractor based on custom header
  let headerKeyExtractor (headerName: string) (ctx: HttpContext) : string =
    match ctx.request.header headerName with
    | Choice1Of2 value -> value
    | Choice2Of2 _ -> defaultKeyExtractor ctx

  /// Create default rate limit configuration
  let defaultConfig =
    { windowSize = TimeSpan.FromMinutes(1.0)
      maxRequests = 60  // 60 requests per minute
      keyExtractor = defaultKeyExtractor
      strategy = FixedWindow
      limitExceededMessage = None
      retryAfter = None }

  /// Check if request is allowed under rate limit
  let private checkRateLimit (config: RateLimitConfig) (ctx: HttpContext) : bool =
    // Ensure cleanup timer is started on first use
    ensureCleanupTimer()
    
    let key = config.keyExtractor ctx
    let now = Globals.utcNow()

    match config.strategy with
    | FixedWindow ->
        globalLimiter.CheckFixedWindow(key, now, config)
    | SlidingWindow ->
        globalLimiter.CheckSlidingWindow(key, now, config)
    | TokenBucket refillRate ->
        globalLimiter.CheckTokenBucket(key, now, config, refillRate)

  /// Rate limiting WebPart that wraps another WebPart
  let rateLimit (config: RateLimitConfig) (protectedPart: WebPart) : WebPart =
    fun (ctx: HttpContext) ->
      async {
        let allowed = checkRateLimit config ctx
        
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
    { defaultConfig with
        windowSize = TimeSpan.FromSeconds(1.0)
        maxRequests = requests }

  /// Create a rate limiter with requests per minute
  let perMinute (requests: int) =
    { defaultConfig with
        windowSize = TimeSpan.FromMinutes(1.0)
        maxRequests = requests }

  /// Create a rate limiter with requests per hour
  let perHour (requests: int) =
    { defaultConfig with
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

  /// Get current rate limit statistics for a key
  let getStats (keyExtractor: HttpContext -> string) (ctx: HttpContext) : Map<string, obj> option =
    let key = keyExtractor ctx
    match globalLimiter.Storage.TryGetValue(key) with
    | true, state ->
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
    | false, _ -> None
