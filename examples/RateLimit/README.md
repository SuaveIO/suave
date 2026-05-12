# Suave Rate Limiting Example

This example demonstrates various rate limiting strategies and configurations available in Suave.

## Running the Example

```bash
cd examples/RateLimit
dotnet run
```

The server will start on `http://localhost:8080`

## Available Endpoints

### Basic Examples

- **`/simple`** - Simple fixed window: 10 requests per minute
- **`/user`** - User-based limiting: 100 requests per minute (use `X-User-Id` header)
- **`/sliding`** - Sliding window: 5 requests per second
- **`/bucket`** - Token bucket: burst of 20, refill 2/second  
- **`/custom`** - Custom session-based: 50 requests per minute (use `X-Session-Id` header)
- **`/stats`** - View rate limit statistics for your IP

### API Tier Examples

- **`/api/public`** - Public tier: 10 requests per minute
- **`/api/user`** - User tier: 100 requests per minute
- **`/api/admin`** - Admin tier: 1000 requests per minute
- **`/api/health`** - Health check: no rate limit

## Testing with curl

### Test Simple Rate Limit
```bash
# This will trigger rate limiting after 10 requests
for i in {1..15}; do
  echo "Request $i:"
  curl http://localhost:8080/simple
  echo -e "\n"
done
```

### Test User-Based Limiting
```bash
# Different users have separate limits
curl -H "X-User-Id: alice" http://localhost:8080/user
curl -H "X-User-Id: bob" http://localhost:8080/user
```

### Test Sliding Window
```bash
# Rapid requests to test per-second limit
for i in {1..10}; do
  curl http://localhost:8080/sliding &
done
wait
```

### Check Your Rate Limit Stats
```bash
curl http://localhost:8080/stats
```

## Expected Responses

### Success Response
```
HTTP/1.1 200 OK
Content-Type: text/plain

Request successful!
```

### Rate Limited Response
```
HTTP/1.1 429 Too Many Requests
Retry-After: 60
X-RateLimit-Limit: 10
Content-Type: text/plain

Rate limit exceeded. Please try again later.
```

## Rate Limiting Strategies

### Fixed Window
- Resets counter at fixed intervals
- Simple and efficient
- Example: `/simple`, `/api/*`

### Sliding Window  
- More accurate than fixed window
- Considers requests in last N seconds
- Example: `/sliding`, `/custom`

### Token Bucket
- Allows controlled bursts
- Refills tokens at steady rate
- Example: `/bucket`

## Key Extractors

The example demonstrates different ways to identify clients:

1. **IP Address** (default): `/simple`, `/sliding`, `/bucket`
2. **User ID**: `/user`, `/api/user` 
3. **Session ID**: `/custom`

## Code Structure

- `Program.fs` - Contains all example configurations
- Each endpoint demonstrates a different feature:
  - `simpleRateLimit` - Basic usage
  - `userRateLimit` - Custom headers + retry-after
  - `slidingWindowLimit` - Sliding window strategy
  - `tokenBucketLimit` - Token bucket with bursts
  - `customKeyLimit` - Custom key extraction
  - `statsEndpoint` - Monitoring

## Learn More

See the [complete rate limiting documentation](../../docs/ratelimit.md) for more details.
