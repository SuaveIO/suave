module Program

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.RateLimit
open System

// Example 1: Simple rate limiting - 10 requests per minute
let simpleRateLimit =
    rateLimit (perMinute 10) (OK "Request successful!")

// Example 2: Rate limit by user (using custom header)
let userRateLimit =
    let config = 
        perMinute 100
        |> withKeyExtractor (headerKeyExtractor "X-User-Id")
        |> withMessage "Too many requests for this user. Please try again later."
        |> withRetryAfter 60
    
    rateLimit config (OK "User request successful!")

// Example 3: Sliding window strategy for more accurate limiting
let slidingWindowLimit =
    let config =
        perSecond 5
        |> withStrategy SlidingWindow
        |> withMessage "Rate limit exceeded. Please slow down."
    
    rateLimit config (OK "Sliding window request successful!")

// Example 4: Token bucket for bursty traffic
let tokenBucketLimit =
    let config =
        { perSecond 10 with
            strategy = TokenBucket 2.0  // Refill 2 tokens per second
            maxRequests = 20 }  // Bucket capacity
        |> withMessage "Bucket exhausted. Please wait for tokens to refill."
    
    rateLimit config (OK "Token bucket request successful!")

// Example 5: Different rate limits for different endpoints
let apiRoutes =
    choose [
        // Public endpoint - strict limit
        path "/api/public" 
        >=> rateLimit 
              (perMinute 10 |> withMessage "Public API rate limit exceeded")
              (OK "Public data")
        
        // Authenticated endpoint - more generous limit
        path "/api/user"
        >=> rateLimit 
              (perMinute 100 |> withKeyExtractor userKeyExtractor)
              (OK "User data")
        
        // Admin endpoint - very generous limit
        path "/api/admin"
        >=> rateLimit (perMinute 1000) (OK "Admin data")
        
        // No rate limit
        path "/api/health"
        >=> OK "OK"
    ]

// Example 6: Rate limiting with custom key extractor
let customKeyLimit =
    let extractSessionId (ctx: HttpContext) =
        match ctx.request.header "X-Session-Id" with
        | Choice1Of2 sessionId -> sessionId
        | Choice2Of2 _ -> defaultKeyExtractor ctx
    
    let config =
        perMinute 50
        |> withKeyExtractor extractSessionId
        |> withStrategy SlidingWindow
    
    rateLimit config (OK "Session-based rate limited request")

// Example 7: Stats endpoint to monitor rate limiting
let statsEndpoint =
    fun ctx ->
        match getStats defaultKeyExtractor ctx with
        | Some stats ->
            let json = 
                stats
                |> Map.toSeq
                |> Seq.map (fun (k, v) -> sprintf "\"%s\": \"%A\"" k v)
                |> String.concat ", "
            OK (sprintf "{ %s }" json) ctx
        | None ->
            OK "{ \"message\": \"No rate limit data found\" }" ctx

// Main application
let app =
    choose [
        // Demo routes with different rate limiting strategies
        path "/" >=> OK "Rate Limiting Examples - Try the endpoints below"
        
        path "/simple" >=> simpleRateLimit
        path "/user" >=> userRateLimit
        path "/sliding" >=> slidingWindowLimit
        path "/bucket" >=> tokenBucketLimit
        path "/custom" >=> customKeyLimit
        path "/stats" >=> statsEndpoint
        
        // API routes with tiered rate limiting
        pathStarts "/api" >=> apiRoutes
        
        RequestErrors.NOT_FOUND "Page not found"
    ]

[<EntryPoint>]
let main argv =
    printfn """
╔═══════════════════════════════════════════════════════════════╗
║              Suave Rate Limiting Example Server              ║
╚═══════════════════════════════════════════════════════════════╝

Available endpoints:

  Simple Rate Limit (10/min):
    http://localhost:8080/simple

  User Rate Limit (100/min):
    http://localhost:8080/user
    (Add X-User-Id header)

  Sliding Window (5/sec):
    http://localhost:8080/sliding

  Token Bucket (burst 20, refill 2/sec):
    http://localhost:8080/bucket

  Custom Session Limit (50/min):
    http://localhost:8080/custom
    (Add X-Session-Id header)

  Rate Limit Stats:
    http://localhost:8080/stats

  API Endpoints (tiered limits):
    http://localhost:8080/api/public   (10/min)
    http://localhost:8080/api/user     (100/min)
    http://localhost:8080/api/admin    (1000/min)
    http://localhost:8080/api/health   (unlimited)

Test with curl or your browser. When rate limited, you'll receive:
  - HTTP 429 (Too Many Requests)
  - Retry-After header (if configured)
  - X-RateLimit-Limit header

Press Ctrl+C to stop the server.
╔═══════════════════════════════════════════════════════════════╗
    """

    startWebServer Web.defaultConfig app
    0
