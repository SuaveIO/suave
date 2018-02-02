module Suave.Tests.Auth


open System
open System.Net
open System.Net.Http
open Expecto
open Suave
open Suave.Logging
open Suave.Logging.Message
open Suave.Cookie
open Suave.State.CookieStateStore
open Suave.Operators
open Suave.Successful
open Suave.Filters
open Suave.RequestErrors
open Suave.Authentication
open Suave.Testing

module Assert =
  let Null (msg : string, o : obj) =
    if o <> null then Tests.failtest msg
    else ()

  let Contains (msg : string, fExpected : 'a -> bool, xs : seq<'a>) =
    if Seq.isEmpty xs then Tests.failtest "empty seq"
    match Seq.tryFind fExpected xs with
    | None -> Tests.failtest msg
    | Some v ->
      // printfn "found %A" v
      ()

let reqResp
  (methd : HttpMethod)
  (resource : string)
  (cookies : CookieContainer option)
  (fRequest : HttpRequestMessage -> HttpRequestMessage)
  fResult
  (ctx : SuaveTestCtx) =

  let event message =
    eventX message >> setSingleName "Suave.Tests"

  let logger =
    ctx.suaveConfig.logger

  logger.debug (
    event "{method} {resource}"
    >> setFieldValue "method" methd
    >> setFieldValue "resource" resource)

  let defaultTimeout = TimeSpan.FromSeconds 5.

  use handler = createHandler DecompressionMethods.None cookies
  use client = createClient handler
  use request = createRequest methd resource "" None (endpointUri ctx.suaveConfig) |> fRequest

  for h in request.Headers do
    logger.debug (event "{headerName}: {headerValue}"
                  >> setFieldValue "headerName" h.Key
                  >> setFieldValue "headerValue" (String.Join(", ", h.Value)))

  // use -> let!!!
  let result = request |> send client defaultTimeout ctx
  fResult result

let setConnectionKeepAlive (r : HttpRequestMessage) =
  r.Headers.ConnectionClose <- Nullable(false)
  r

/// Test a request by looking at the cookies alone.
let reqCookies cookies ctx methd resource fReq =
  reqResp methd resource (Some cookies)
           setConnectionKeepAlive
           fReq
           ctx

let cookies suaveConfig (container : CookieContainer) =
  container.GetCookies(endpointUri suaveConfig)

let interaction ctx fCtx = withContext fCtx ctx

let interact methd resource container ctx =
  let response = reqCookies container ctx methd resource id
  match response.Headers.TryGetValues("Set-Cookie") with
  | false, _ -> ()
  | true, values -> values |> Seq.iter (fun cookie -> container.SetCookies(endpointUri ctx.suaveConfig, cookie))
  response

let sessionState f =
  context( fun r ->
    match HttpContext.state r with
    | None ->  RequestErrors.BAD_REQUEST "damn it"
    | Some store -> f store )

[<Tests>]
let authTests cfg =
  let runWithConfig = runWith { cfg with logger = Targets.create Warn [||] }
  testList "auth tests" [
    testCase "baseline, no auth cookie" <| fun _ ->
      let ctx = runWithConfig (OK "ACK")
      let cookies = ctx |> reqCookies' HttpMethod.GET "/"  None
      Assert.Null("should not have auth cookie", cookies.[SessionAuthCookie])

    testCase "can set cookie" <| fun _ ->
      let ctx = runWithConfig (authenticated Session false >=> OK "ACK")
      let cookies = ctx |> reqCookies' HttpMethod.GET "/"  None
      Expect.isNotNull cookies.[SessionAuthCookie] "Should have auth cookie"

    testCase "can set MaxAge cookie" <| fun _ ->
      let timespan = System.TimeSpan.FromDays(13.0)
      let maxAge = Cookie.CookieLife.MaxAge timespan
      let ctx = runWithConfig (authenticated maxAge false >=> OK "ACK")
      let cookies = ctx |> reqCookies' HttpMethod.GET "/"  None
      Expect.isNotNull cookies.[SessionAuthCookie] "should have auth cookie"

    testCase "can access session id when authenticate" <| fun _ ->
      let readSessionId = context (HttpContext.sessionId >> function
        | None -> OK "no session id"
        | Some _ -> OK "session id found")
      let res =
        runWithConfig (authenticated Session false >=> readSessionId)
        |> req HttpMethod.GET "/" None
      Expect.equal res "session id found" "should find session id"

    testCase "can access authenticated contents when authenticate, and not after deauthenticate" <| fun _ ->
      // given
      let ctx =
        runWithConfig (
          choose [
            path "/" >=> OK "root"
            path "/auth" >=> authenticated Session false >=> OK "authed"
            path "/protected"
              >=> authenticate Session false
                               (fun () ->
                                 Choice2Of2(FORBIDDEN "please authenticate"))
                               (fun _ -> Choice2Of2(BAD_REQUEST "did you fiddle with our cipher text?"))
                               (OK "You have reached the place of your dreams!")
            path "/deauth" >=> deauthenticate >=> OK "deauthed"
            NOT_FOUND "arghhh"
            ])

      // mutability bonanza here:
      let container = CookieContainer()
      let interact methd resource = interact methd resource container ctx
      let cookies = cookies ctx.suaveConfig container

      // when
      interaction ctx <| fun _ ->
        use res = interact HttpMethod.GET "/"
        let actual = contentString res
        Expect.equal actual "root" "should allow root request"

        match cookies.[SessionAuthCookie] with
        | null -> ()
        | cookie -> Tests.failtestf "should not have auth cookie, but was %A" cookie

        use res' = interact HttpMethod.GET "/protected"
        Expect.equal (contentString res') "please authenticate" "should not have access to protected"
        Expect.equal (statusCode res') HttpStatusCode.Forbidden "code 403 FORBIDDEN"

        use res'' = interact HttpMethod.GET "/auth"
        Assert.Contains("after authentication", (fun (str : string) -> str.Contains("auth=")),
                                                res''.Headers.GetValues "Set-Cookie")
        Expect.equal (contentString res'') "authed" "after authentication"

        use res''' = interact HttpMethod.GET "/protected"
        Expect.equal (contentString res''') "You have reached the place of your dreams!" "should have access to protected"
        Expect.equal (statusCode res''') HttpStatusCode.OK "code 200 OK"

        use res'''' = interact HttpMethod.GET "/deauth"
        Expect.equal (contentString res'''') "deauthed" "should have logged out now"

        use res''''' = interact HttpMethod.GET "/protected"
        Expect.equal (contentString res''''') "please authenticate" "should not have access to protected after logout"

    testCase "test session is maintained across requests" <| fun _ ->
      // given
      let ctx =
        runWithConfig (
          statefulForSession
          >=> sessionState (fun store ->
              match store.get "counter" with
              | Some y ->
                store.set "counter" (y + 1)
                >=> OK ((y + 1).ToString())
              | None ->
                store.set "counter" 0
                >=> OK "0"))

      let container = CookieContainer()
      let interact methd resource = interact methd resource container ctx

      interaction ctx  (fun _ ->
        use res = interact HttpMethod.GET "/"
        Expect.equal (contentString res) "0" "should return number zero"

        use res' = interact HttpMethod.GET "/"
        Expect.equal (contentString res') "1" "should return number one"

        use res'' = interact HttpMethod.GET "/"
        Expect.equal (contentString res'') "2" "should return number two")

    testCase "set more than one variable in the session" <| fun _ ->
      // given
      let ctx =
        runWithConfig (
          statefulForSession
          >=> choose [
            path "/a"     >=> sessionState (fun state -> state.set "a" "a" >=> OK "a" )
            path "/b"     >=> sessionState (fun state -> state.set "b" "b" >=> OK "b" )
            path "/get_a" >=> sessionState (fun state -> match state.get "a" with Some a -> OK a | None -> RequestErrors.BAD_REQUEST "fail")
            path "/get_b" >=> sessionState (fun state -> match state.get "b" with Some a -> OK a | None -> RequestErrors.BAD_REQUEST "fail" )
            ])

      let container = CookieContainer()
      let interact methd resource = interact methd resource container ctx

      interaction ctx  (fun _ ->
        use res = interact HttpMethod.GET "/a"
        Expect.equal (contentString res) "a" "should return a"

        use res' = interact HttpMethod.GET "/b"
        Expect.equal (contentString res') "b" "should return b"

        use res'' = interact HttpMethod.GET "/get_a"
        Expect.equal (contentString res'') "a" "should return a"

        use res''' = interact HttpMethod.GET "/get_b"
        Expect.equal (contentString res''') "b" "should return b")

    testCase "set two session values on the same request" <| fun _ ->
      // given
      let ctx =
        runWithConfig (
          statefulForSession >=> choose [
            path "/ab"     >=> sessionState (fun state -> state.set "a" "a" >=> sessionState ( fun state' -> state'.set "b" "b" >=> OK "a" ))
            path "/get_a" >=> sessionState (fun state -> match state.get "a" with Some a -> OK a | None -> RequestErrors.BAD_REQUEST "fail")
            path "/get_b" >=> sessionState (fun state -> match state.get "b" with Some a -> OK a | None -> RequestErrors.BAD_REQUEST "fail" )
            ])

      let container = CookieContainer()
      let interact methd resource = interact methd resource container ctx

      interaction ctx  (fun _ ->
        use res = interact HttpMethod.GET "/ab"
        Expect.equal (contentString res) "a" "should return a"

        use res''' = interact HttpMethod.GET "/get_b"
        Expect.equal (contentString res''') "b" "should return b"

        use res'' = interact HttpMethod.GET "/get_a"
        Expect.equal (contentString res'') "a" "should return a")
    ]