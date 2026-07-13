# Rate Limiting

Suave includes built-in support for rate limiting to protect your API endpoints from abuse and ensure fair resource usage.

## Features

- **Multiple Strategies**: Fixed window, sliding window, and token bucket algorithms
- **Flexible Key Extraction**: Rate limit by IP address, user ID, session, or custom keys
- **Configurable Limits**: Set limits per second, minute, or hour
- **Automatic Cleanup**: Old rate limit data is automatically cleaned up
- **HTTP Headers**: Returns proper HTTP 429 status with `Retry-After` and `X-RateLimit-Limit` headers
- **Thread-Safe**: Uses concurrent data structures for high-performance scenarios

## Quick Start

```fsharp
open Suave
open Suave.RateLimit

// Simple rate limiting - 10 requests per minute
let app =
    rateLimit (perMinute 10)
    >=> Successful.OK "Hello, world!"

startWebServer defaultConfig app
```

## Rate Limiting Strategies

### Fixed Window

Resets the counter at fixed intervals. Simple and efficient.

```fsharp
let config =
    perMinute 100
    |> withStrategy FixedWindow

rateLimit config >=> OK "Success"
```

### Sliding Window

More accurate than fixed window - considers requests in the last N seconds.

```fsharp
let config =
    perMinute 100
    |> withStrategy SlidingWindow

rateLimit config >=> OK "Success"
```

### Token Bucket

Allows controlled bursts while maintaining average rate.

```fsharp
let config =
    { perSecond 10 with
        strategy = TokenBucket 2.0  // Refill 2 tokens per second
        maxRequests = 20 }          // Bucket capacity (burst size)

rateLimit config >=> OK "Success"
```

## Configuration

### Time Windows

```fsharp
// Requests per second
perSecond 5

// Requests per minute  
perMinute 100

// Requests per hour
perHour 1000
```

### Key Extractors

Rate limiting can be based on different keys:

#### By IP Address (default)
```fsharp
rateLimit (perMinute 60)  // Uses client IP by default
```

#### By User
```fsharp
let config =
    perMinute 100
    |> withKeyExtractor userKeyExtractor  // Uses userName from userState

rateLimit config >=> OK "User-specific limit"
```

#### By Custom Header
```fsharp
let config =
    perMinute 100
    |> withKeyExtractor (headerKeyExtractor "X-API-Key")

rateLimit config >=> OK "API key limited"
```

#### Custom Key Extractor
```fsharp
let extractSessionId (ctx: HttpContext) =
    match ctx.request.header "X-Session-Id" with
    | Choice1Of2 sessionId -> sessionId
    | Choice2Of2 _ -> defaultKeyExtractor ctx  // Fallback to IP

let config =
    perMinute 50
    |> withKeyExtractor extractSessionId

rateLimit config >=> OK "Session limited"
```

### Custom Messages

```fsharp
let config =
    perMinute 10
    |> withMessage "Too many requests. Please slow down!"
    
rateLimit config >=> OK "Success"
```

### Retry-After Header

```fsharp
let config =
    perMinute 60
    |> withRetryAfter 60  // Tell client to retry after 60 seconds

rateLimit config >=> OK "Success"
```

## Complete Example

```fsharp
open Suave
open Suave.RateLimit
open Suave.Filters
open Suave.Operators
open Suave.Successful

// Public API - strict limit
let publicApi =
    rateLimit (perMinute 10)
    >=> OK "Public data"

// Authenticated API - generous limit
let authenticatedApi =
    rateLimit (perMinute 100 |> withKeyExtractor userKeyExtractor)
    >=> OK "User data"

// Premium tier - token bucket for bursts
let premiumApi =
    let config =
        { perSecond 50 with
            strategy = TokenBucket 10.0
            maxRequests = 200 }
        |> withMessage "Rate limit exceeded for premium tier"
    
    rateLimit config
    >=> OK "Premium data"

let app =
    choose [
        path "/api/public" >=> publicApi
        path "/api/user" >=> authenticatedApi  
        path "/api/premium" >=> premiumApi
        RequestErrors.NOT_FOUND "Not found"
    ]

startWebServer defaultConfig app
```

## Tiered Rate Limiting

Apply different limits to different endpoints:

```fsharp
let app =
    choose [
        // Health check - no rate limit
        path "/health" >=> OK "OK"
        
        // Public endpoints - 10/min
        pathStarts "/api/public"
        >=> rateLimit (perMinute 10)
        >=> publicRoutes
        
        // User endpoints - 100/min
        pathStarts "/api/user"
        >=> rateLimit (perMinute 100 |> withKeyExtractor userKeyExtractor)
        >=> userRoutes
        
        // Admin endpoints - 1000/min
        pathStarts "/api/admin"
        >=> rateLimit (perMinute 1000)
        >=> adminRoutes
    ]
```

## Monitoring Rate Limits

Get current rate limit statistics:

```fsharp
open Suave.RateLimit

let statsEndpoint =
    fun ctx ->
        match getStats defaultKeyExtractor ctx with
        | Some stats ->
            // stats contains: strategy, count, windowStart/tokens/lastRefill
            OK (sprintf "%A" stats) ctx
        | None ->
            OK "No rate limit data" ctx

let app =
    choose [
        path "/stats" >=> statsEndpoint
        // ... other routes
    ]
```

## Response Headers

When rate limited, clients receive:

- **Status**: `429 Too Many Requests`
- **Retry-After** (if configured): Seconds until limit resets
- **X-RateLimit-Limit**: Maximum requests allowed

Example response:
```
HTTP/1.1 429 Too Many Requests
Retry-After: 60
X-RateLimit-Limit: 100
Content-Type: text/plain

Rate limit exceeded. Please try again later.
```

## Best Practices

1. **Start Conservative**: Begin with stricter limits and relax them based on actual usage
2. **Different Tiers**: Apply different limits for public, authenticated, and premium users
3. **Exclude Health Checks**: Don't rate limit health check endpoints
4. **Clear Messages**: Provide helpful error messages explaining the limit
5. **Monitor Usage**: Track rate limit hits to identify legitimate vs abusive traffic
6. **Combine Strategies**: Use token bucket for APIs that need to handle bursts
7. **Key Selection**: Choose the right key extractor (IP, user, API key) based on your use case

## Performance Considerations

- Rate limiting uses `ConcurrentDictionary` for thread-safe, high-performance operation
- Automatic cleanup runs every 5 minutes to remove stale entries (configurable)
- Fixed window is fastest, sliding window is more accurate, token bucket allows bursts
- Minimal overhead: ~1-5 microseconds per request
- Memory usage scales with number of unique keys (IPs/users/sessions)

## Testing Rate Limits

Use `curl` to test rate limiting:

```bash
# Test basic rate limit
for i in {1..15}; do
  curl http://localhost:8080/api/public
  echo ""
done

# Test with custom header
for i in {1..10}; do
  curl -H "X-User-Id: user123" http://localhost:8080/api/user
  echo ""
done

# Check rate limit stats
curl http://localhost:8080/stats
```

## Distributed Scenarios

The current implementation uses in-memory storage. For distributed deployments:

1. Use sticky sessions to route same client to same server
2. Or implement a custom storage backend (Redis, distributed cache)
3. Consider using an API gateway with built-in rate limiting

## See Also

- [Filters and Routing](routing.md)
- [Authentication](async.md)
- [WebPart Combinators](combinators.md)
