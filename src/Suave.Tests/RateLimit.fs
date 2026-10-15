module Suave.Tests.RateLimit

open System
open System.Net
open System.Net.Http
open Suave
open Suave.Operators
open Suave.Successful
open Suave.RateLimit
open Suave.Tests.TestUtilities
open Suave.Testing
open Expecto

// Helper functions to maintain client state across requests
let interact methd resource container ctx =
  let defaultTimeout = TimeSpan.FromSeconds 5.
  use handler = createHandler DecompressionMethods.None (Some container)
  use client = createClient handler
  use request = createRequest methd resource "" None (endpointUri ctx.suaveConfig)
  request.Headers.ConnectionClose <- Nullable(false)
  let response = request |> send client defaultTimeout ctx
  response

let interaction ctx fCtx = withContext fCtx ctx

[<Tests>]
let basicRateLimiting cfg =
  let runWithConfig = runWith cfg
  
  testList "basic rate limiting" [
    testCase "allows requests under limit" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config = 
        perSecond 5
        |> withKeyExtractor (fun _ -> testId)
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // Make 5 requests - all should succeed
        for i in 1..5 do
          use res = interact' HttpMethod.GET "/"
          let result = contentString res
          Expect.equal result "success" (sprintf "request %d should succeed" i)
    
    testCase "blocks request over limit with 429" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config =
        perSecond 3
        |> withKeyExtractor (fun _ -> testId)
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // Make 3 requests - should all succeed
        for i in 1..3 do
          use res = interact' HttpMethod.GET "/"
          let result = contentString res
          Expect.equal result "success" (sprintf "request %d should succeed" i)
        
        // 4th request should be rate limited
        use res = interact' HttpMethod.GET "/"
        let status = statusCode res
        Expect.equal status HttpStatusCode.TooManyRequests "4th request should return 429"
    
    testCase "returns correct headers when rate limited" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config = 
        perSecond 2
        |> withRetryAfter 60
        |> withKeyExtractor (fun _ -> testId)
      
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // Exhaust the limit
        for i in 1..2 do
          use res = interact' HttpMethod.GET "/"
          ()
        
        // Get headers from rate-limited response
        use res = interact' HttpMethod.GET "/"
        let hasRateLimitHeader = 
          res.Headers
          |> Seq.exists (fun h -> h.Key = "X-RateLimit-Limit")
        
        Expect.isTrue hasRateLimitHeader "should have X-RateLimit-Limit header"
    
    testCase "custom error message is returned" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config =
        perSecond 1
        |> withMessage "Slow down! Try again later."
        |> withKeyExtractor (fun _ -> testId)
      
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // First request succeeds
        use res1 = interact' HttpMethod.GET "/"
        ()
        
        // Second request gets custom message
        use res2 = interact' HttpMethod.GET "/"
        let result = contentString res2
        Expect.equal result "Slow down! Try again later." "should return custom message"
  ]

[<Tests>]
let fixedWindowStrategy cfg =
  let runWithConfig = runWith cfg
  
  testList "fixed window strategy" [
    testCase "window resets after time expires" <| fun _ ->
      // Use a very short window for faster testing
      let testId = System.Guid.NewGuid().ToString()
      let config = 
        { perSecond 2 with 
            windowSize = TimeSpan.FromMilliseconds(500.0)
            keyExtractor = fun _ -> testId }
      
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // Exhaust limit
        for i in 1..2 do
          use res = interact' HttpMethod.GET "/"
          ()
        
        // Should be rate limited
        use res1 = interact' HttpMethod.GET "/"
        let status1 = statusCode res1
        Expect.equal status1 HttpStatusCode.TooManyRequests "should be rate limited"
        
        // Wait for window to reset
        Threading.Thread.Sleep(600)
        
        // Should succeed after window reset
        use res2 = interact' HttpMethod.GET "/"
        let result = contentString res2
        Expect.equal result "success" "should succeed after window reset"
    
    testCase "counts increment correctly" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config =
        perSecond 10
        |> withKeyExtractor (fun _ -> testId)
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // Make several requests
        for i in 1..7 do
          use res = interact' HttpMethod.GET "/"
          let result = contentString res
          Expect.equal result "success" (sprintf "request %d should succeed" i)
  ]

[<Tests>]
let slidingWindowStrategy cfg =
  let runWithConfig = runWith cfg
  
  testList "sliding window strategy" [
    testCase "sliding window allows request after partial window" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config =
        { perSecond 2 with 
            windowSize = TimeSpan.FromMilliseconds(1000.0)
            strategy = SlidingWindow
            keyExtractor = fun _ -> testId }
      
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // Make 2 requests quickly
        for i in 1..2 do
          use res = interact' HttpMethod.GET "/"
          ()
        
        // Should be rate limited
        use res1 = interact' HttpMethod.GET "/"
        let status1 = statusCode res1
        Expect.equal status1 HttpStatusCode.TooManyRequests "should be rate limited"
        
        // Wait for oldest request to fall out of window
        Threading.Thread.Sleep(1100)
        
        // Should succeed as old requests expired
        use res2 = interact' HttpMethod.GET "/"
        let result = contentString res2
        Expect.equal result "success" "should succeed after old requests expired"
  ]

[<Tests>]
let tokenBucketStrategy cfg =
  let runWithConfig = runWith cfg
  
  testList "token bucket strategy" [
    testCase "allows burst up to capacity" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config =
        { perSecond 1 with
            maxRequests = 5  // Bucket capacity
            strategy = TokenBucket 1.0  // Refill 1 token/sec
            keyExtractor = fun _ -> testId }
      
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // Should be able to make 5 requests immediately (burst)
        for i in 1..5 do
          use res = interact' HttpMethod.GET "/"
          let result = contentString res
          Expect.equal result "success" (sprintf "burst request %d should succeed" i)
        
        // 6th request should be rate limited
        use res = interact' HttpMethod.GET "/"
        let status = statusCode res
        Expect.equal status HttpStatusCode.TooManyRequests "6th request should be rate limited"
    
    testCase "tokens refill over time" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config =
        { perSecond 1 with
            maxRequests = 3
            strategy = TokenBucket 2.0  // Refill 2 tokens/sec
            keyExtractor = fun _ -> testId }
      
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // Exhaust bucket
        for i in 1..3 do
          use res = interact' HttpMethod.GET "/"
          ()
        
        // Should be rate limited
        use res1 = interact' HttpMethod.GET "/"
        let status1 = statusCode res1
        Expect.equal status1 HttpStatusCode.TooManyRequests "should be rate limited"
        
        // Wait for refill (2 tokens at 2/sec means 1 second)
        Threading.Thread.Sleep(1100)
        
        // Should have refilled ~2 tokens
        use res2 = interact' HttpMethod.GET "/"
        let result = contentString res2
        Expect.equal result "success" "should succeed after token refill"
  ]

[<Tests>]
let edgeCases cfg =
  let runWithConfig = runWith cfg
  
  testList "edge cases" [
    testCase "zero requests allowed blocks immediately" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config = 
        { perSecond 1 with 
            maxRequests = 0
            keyExtractor = fun _ -> testId }
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        use res = interact' HttpMethod.GET "/"
        let status = statusCode res
        Expect.equal status HttpStatusCode.TooManyRequests "should be rate limited immediately"
    
    testCase "single request allowed works" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config =
        perSecond 1
        |> withKeyExtractor (fun _ -> testId)
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // First request succeeds
        use res1 = interact' HttpMethod.GET "/"
        let result1 = contentString res1
        Expect.equal result1 "success" "first request should succeed"
        
        // Second request blocked
        use res2 = interact' HttpMethod.GET "/"
        let status = statusCode res2
        Expect.equal status HttpStatusCode.TooManyRequests "second request should be rate limited"
    
    testCase "very high limit allows many requests" <| fun _ ->
      let testId = System.Guid.NewGuid().ToString()
      let config =
        perSecond 1000
        |> withKeyExtractor (fun _ -> testId)
      let app = rateLimit config (OK "success")
      let ctx = runWithConfig app
      
      let container = CookieContainer()
      let interact' methd resource = interact methd resource container ctx
      
      interaction ctx <| fun _ ->
        // Make 50 requests - all should succeed
        for i in 1..50 do
          use res = interact' HttpMethod.GET "/"
          let result = contentString res
          Expect.equal result "success" (sprintf "request %d should succeed" i)
  ]

// Sends a request with the given headers and returns its status code
let statusWithHeaders resource (headers : (string * string) list) ctx =
  use handler = createHandler DecompressionMethods.None None
  use client = createClient handler
  use request = createRequest HttpMethod.GET resource "" None (endpointUri ctx.suaveConfig)
  for (name, value) in headers do
    request.Headers.TryAddWithoutValidation(name, value) |> ignore
  use response = request |> send client (TimeSpan.FromSeconds 5.) ctx
  response.StatusCode

[<Tests>]
let limiterIsolation cfg =
  let runWithConfig = runWith cfg

  testList "limiter isolation" [
    testCase "limiters with the same key keep separate counters" <| fun _ ->
      let config = perMinute 1 |> withKeyExtractor (fun _ -> "shared-key")
      let app =
        choose [
          Filters.path "/a" >=> rateLimit config (OK "a")
          Filters.path "/b" >=> rateLimit config (OK "b") ]
      let ctx = runWithConfig app

      interaction ctx <| fun _ ->
        Expect.equal (statusWithHeaders "/a" [] ctx) HttpStatusCode.OK "first /a allowed"
        Expect.equal (statusWithHeaders "/b" [] ctx) HttpStatusCode.OK "first /b allowed: /a's count is not shared"
        Expect.equal (statusWithHeaders "/a" [] ctx) HttpStatusCode.TooManyRequests "second /a limited"

    testCase "limiters with different strategies on the same key both allow" <| fun _ ->
      let app =
        choose [
          Filters.path "/sliding" >=> rateLimit (perMinute 5 |> withStrategy SlidingWindow) (OK "sliding")
          Filters.path "/fixed" >=> rateLimit (perMinute 5 |> withStrategy FixedWindow) (OK "fixed")
          Filters.path "/bucket" >=> rateLimit (perMinute 5 |> withStrategy (TokenBucket 1.0)) (OK "bucket") ]
      let ctx = runWithConfig app

      interaction ctx <| fun _ ->
        Expect.equal (statusWithHeaders "/sliding" [] ctx) HttpStatusCode.OK "sliding allowed"
        Expect.equal (statusWithHeaders "/fixed" [] ctx) HttpStatusCode.OK "fixed allowed after sliding"
        Expect.equal (statusWithHeaders "/bucket" [] ctx) HttpStatusCode.OK "bucket allowed after fixed"
  ]

[<Tests>]
let keyExtractors cfg =
  let runWithConfig = runWith cfg
  let loopbacks = [ IPAddress.Loopback; IPAddress.IPv6Loopback ]

  testList "key extractors" [
    testCase "default key extractor ignores X-Forwarded-For" <| fun _ ->
      let app = rateLimit (perMinute 2) (OK "success")
      let ctx = runWithConfig app

      interaction ctx <| fun _ ->
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "203.0.113.1" ] ctx) HttpStatusCode.OK "first allowed"
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "203.0.113.2"; "X-Real-IP", "203.0.113.2" ] ctx) HttpStatusCode.OK "second allowed"
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "203.0.113.3"; "X-Real-IP", "203.0.113.3" ] ctx) HttpStatusCode.TooManyRequests "spoofed headers do not reset the limit"

    testCase "trusted proxy extractor keys by forwarded client address" <| fun _ ->
      let app = rateLimit (perMinute 1 |> withKeyExtractor (trustedProxyKeyExtractor loopbacks)) (OK "success")
      let ctx = runWithConfig app

      interaction ctx <| fun _ ->
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "203.0.113.1" ] ctx) HttpStatusCode.OK "client 1 allowed"
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "203.0.113.2" ] ctx) HttpStatusCode.OK "client 2 allowed"
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "203.0.113.1" ] ctx) HttpStatusCode.TooManyRequests "client 1 limited"

    testCase "trusted proxy extractor ignores entries added by the client" <| fun _ ->
      let app = rateLimit (perMinute 1 |> withKeyExtractor (trustedProxyKeyExtractor loopbacks)) (OK "success")
      let ctx = runWithConfig app

      interaction ctx <| fun _ ->
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "198.51.100.1, 203.0.113.9" ] ctx) HttpStatusCode.OK "first allowed"
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "198.51.100.2, 203.0.113.9" ] ctx) HttpStatusCode.TooManyRequests "keyed by the proxy-appended address"

    testCase "trusted proxy extractor ignores headers from untrusted connections" <| fun _ ->
      let app = rateLimit (perMinute 1 |> withKeyExtractor (trustedProxyKeyExtractor [ IPAddress.Parse "192.0.2.10" ])) (OK "success")
      let ctx = runWithConfig app

      interaction ctx <| fun _ ->
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "203.0.113.1" ] ctx) HttpStatusCode.OK "first allowed"
        Expect.equal (statusWithHeaders "/" [ "X-Forwarded-For", "203.0.113.2" ] ctx) HttpStatusCode.TooManyRequests "header ignored, keyed by connection"
  ]
