module Suave.Tests.HttpAuthentication

open System
open System.Net
open System.Net.Http.Headers
open System.Text
open Suave
open Suave.Operators
open Suave.Filters
open Suave.Authentication
open Suave.Successful
open Suave.ServerErrors

open Suave.Tests.TestUtilities
open Suave.Testing

open Expecto

[<Tests>]
let authTests cfg =
  let runWithConfig = runWith cfg

  let user = "foo"
  let pasw = "bar"
  let basicCredentials = sprintf "%s:%s" user pasw |> Encoding.Default.GetBytes |> Convert.ToBase64String

  let getUserName ctx =
    match ctx.userState.TryGetValue UserNameKey  with
    | true, username -> sprintf "hello %O" username
    | false,_ -> "no user"

  let okUser =
    context(fun ctx -> OK(getUserName ctx))

  let app =
    choose [
      GET >=> path "/non-protected1" >=> okUser
      GET >=> path "/protected" >=> authenticateBasic ((=) ("foo", "bar")) okUser
      GET >=> path "/non-protected2" >=> okUser ]

  testList "basic authetication" [
    let req path reqMod = reqResp HttpMethod.GET path "" None None DecompressionMethods.None reqMod (fun res -> int res.StatusCode, res.Content.ReadAsStringAsync().Result)

    testCase "add username to userstate for protectedPart only" <| fun _ ->
      let _, res = runWithConfig app |> req "/non-protected1" id
      Expect.equal res "no user" "access to /non-protected1 should result in no username"

      let _, res = runWithConfig app |> req "/non-protected2" id
      Expect.equal res "no user" "access to /non-protected2 should result in no username"

      let _, res = runWithConfig app |> req "/protected" (fun reqmsg -> reqmsg.Headers.Authorization <- AuthenticationHeaderValue("Basic", basicCredentials); reqmsg)
      Expect.equal res "hello foo" "should be username"

      let _, res = runWithConfig app |> req "/non-protected1" id
      Expect.equal res "no user" "access to /non-protected1 should result in no username (2)"

      let _, res = runWithConfig app |> req "/non-protected2" id
      Expect.equal res "no user" "access to /non-protected2 should result in no username (2)"

    testCase "invalid Authorization token leads to challenge" <| fun _ ->
      let code, _ = runWithConfig app |> req "/protected" (fun reqmsg -> reqmsg.Headers.Authorization <- AuthenticationHeaderValue("Basic", "totallyinvalid"); reqmsg)
      Expect.equal code 401 "should be challenged"

    testCase "add username to userstate for protectedPart (async)" <| fun _ ->

      let authenticate credentials = async { return credentials = ("foo", "bar") }
      let app = GET >=> authenticateBasicAsync authenticate okUser

      let _, res = runWithConfig app |> req "/protected" (fun reqmsg -> reqmsg.Headers.Authorization <- AuthenticationHeaderValue("Basic", basicCredentials); reqmsg)
      Expect.equal res "hello foo" "should be username"

    ]
