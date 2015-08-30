module Suave.Tests.Owin

open Fuchu

open System
open System.Collections.Generic
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text

open Suave
open Suave.Types
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Writers
open Suave.Owin

open Suave.Tests.TestUtilities
open Suave.Testing

let eq msg a b =
  Assert.Equal(msg, a, b)

let eqs msg bs aas =
  Assert.Equal(msg, bs |> List.ofSeq, aas |> List.ofSeq)

let throws msg matcher fn =
  try fn () with e when matcher e -> ()

[<Tests>]
let unit =
  let create (m : (string * string) list) =
    OwinAppFunc.DeltaDictionary(m)

  testList "infrastructure" [
    testList "DeltaDictionary" [
      testCase "construct & Delta" <| fun () ->
        let subject = create ["a", "a-1"]
        eq "has a" [|"a-1"|] subject.Delta.["a"]

      testCase "interaction set/remove" <| fun _ ->
        let subject = create ["a", "a-1"]
        eqs "has a" ["a-1"] subject.Delta.["a"]

        eqs "has only a"
           ([ "a" ] :> _ seq)
           ((subject.Delta :> IDictionary<_, _>).Keys :> _ seq)

        (subject :> IDictionary<_, _>).["b"] <- [| "b-1"; "b-2" |]
        eqs "has b" ["b-1"; "b-2"] subject.Delta.["b"]

        eq "can remove b once" true ((subject :> IDictionary<_, _>).Remove("b"))
        throws "key not found exception on b"
               (function :? KeyNotFoundException -> true | _ -> false)
               (fun _ -> subject.Delta.["b"] |> ignore)
        eq "cannot remove b twice" false ((subject :> IDictionary<_, _>).Remove("b"))

        (subject :> IDictionary<_, _>).["b"] <- [| "b-3" |]
        eqs "has b once more" ["b-3"] subject.Delta.["b"]

        (subject :> IDictionary<_, _>).["b"] <- [| "b-4" |]
        eqs "can change b after remove" ["b-4"] ((subject.Delta :> IDictionary<_, _>).["b"])
        eq "can remove b after change b after remove" true ((subject :> IDictionary<_, _>).Remove("b"))
        
        (subject :> IDictionary<_, _>).["c"] <- [| "c-1" |]
        eqs "has a, c"
            ["a"; "c"]
            ((subject.Delta :> IDictionary<_, _>).Keys :> _ seq)

        eq "can remove a once" true ((subject :> IDictionary<_, _>).Remove("a"))
        eq "cannot remove a twice" false ((subject :> IDictionary<_, _>).Remove("a"))

        eq "cannot remove what's never been there" false ((subject :> IDictionary<_, _>).Remove("x"))
      ]
    ]

[<Tests>]
let endToEnd =
  let runWithConfig = runWith defaultConfig

  let owinHelloWorld (env : OwinEnvironment) =
    let hello = "Hello, OWIN!"B

    env.[OwinConstants.responseStatusCode] <- box 201

    // set content type, new reference, invalid charset
    let responseHeaders : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
    responseHeaders.["Content-Type"] <- [| "application/json; charset=utf-1" |]

    // overwrite invalid 1, new reference, invalid charset
    let responseHeaders' : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
    responseHeaders'.["Content-Type"] <- [| "text/plain; charset=utf-2" |]

    // overwrite invalid 2, old reference, invalid charset
    responseHeaders.["Content-Type"] <- [| "application/json; charset=utf-3" |]

    // overwrite final, new reference
    let responseHeaders'' : IDictionary<string, string[]> = unbox env.[OwinConstants.responseHeaders]
    responseHeaders''.["Content-Type"] <- [| "text/plain; charset=utf-8" |]

    let responseStream : IO.Stream = unbox env.[OwinConstants.responseBody]
    responseStream.Write(hello, 0, hello.Length)
    async.Return ()

  let composedApp =
    path "/owin"
      >>= setHeader "X-Custom-Before" "Before OWIN"
      >>= OwinAppFunc.ofOwin owinHelloWorld
      >>= setHeader "X-Custom-After" "After OWIN"

  testList "e2e" [
    testCase "Hello, OWIN!" <| fun _ ->
      let asserts (result : HttpResponseMessage) =
        eq "Content-Type" "text/plain; charset=utf-8" (result.Content.Headers.ContentType.ToString())
        eq "Http Status Code" HttpStatusCode.Created result.StatusCode
        eq "Content Length" ("Hello, OWIN!"B.LongLength) (result.Content.Headers.ContentLength.Value)
        eq "Contents" "Hello, OWIN!" (result.Content.ReadAsStringAsync().Result)

        match result.Headers.TryGetValues("X-Custom-Before") with
        | true, actual ->
          eqs "Headers set before the OWIN app func, are sent"
              ["Before OWIN"]
              actual
        | false, _ -> Tests.failtest "X-Custom-Before is missing"

        match result.Headers.TryGetValues("X-Custom-After") with
        | true, actual ->
          eqs "Headers after before the OWIN app func, are sent"
              ["After OWIN"]
              actual
        | false, _ -> Tests.failtest "X-Custom-After is missing"

      runWithConfig composedApp |> reqResp Types.GET "/owin" "" None None DecompressionMethods.GZip id asserts
    ]
    