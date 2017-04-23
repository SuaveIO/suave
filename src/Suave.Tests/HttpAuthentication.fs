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

  testList "basic authetication" [
    testCase "add username to userstate for protectedPart only" <| fun _ ->
      let okUser = context <| fun ctx ->
        match Map.tryFind UserNameKey ctx.userState with
        | Some username -> sprintf "hello %O" username
        | None -> "no user"
        |> OK

      let app =
        choose [
          GET >=> path "/non-protected1" >=> okUser
          GET >=> path "/protected" >=> authenticateBasic ((=) ("foo", "bar")) okUser
          GET >=> path "/non-protected2" >=> okUser ]

      let req path reqMod = reqResp HttpMethod.GET path "" None None DecompressionMethods.None reqMod (fun res -> res.Content.ReadAsStringAsync().Result)

      let res = runWithConfig app |> req "/protected" (fun reqmsg -> reqmsg.Headers.Authorization <- AuthenticationHeaderValue("Basic", basicCredentials); reqmsg)
      Expect.equal res "hello foo" "should be username"

      let res = runWithConfig app |> req "/non-protected1" id
      Expect.equal res "no user" "should be no username"

      let res = runWithConfig app |> req "/non-protected2" id
      Expect.equal res "no user" "should be no username"

    testCase "add username to userstate for protectedPart (async)" <| fun _ ->
      let okUser = context <| fun ctx ->
        match Map.tryFind UserNameKey ctx.userState with
        | Some username -> sprintf "hello %O" username
        | None -> "no user"
        |> OK

      let authenticate credentials = async { return credentials = ("foo", "bar") }
      let app = GET >=> authenticateBasicAsync authenticate okUser

      let req path reqMod = reqResp HttpMethod.GET path "" None None DecompressionMethods.None reqMod (fun res -> res.Content.ReadAsStringAsync().Result)

      let res = runWithConfig app |> req "/protected" (fun reqmsg -> reqmsg.Headers.Authorization <- AuthenticationHeaderValue("Basic", basicCredentials); reqmsg)
      Expect.equal res "hello foo" "should be username"

    ]
