module Suave.Tests.Types

open System
open System.Net
open Expecto
open Suave.Sockets
open Suave
open System.Net.Http
open Suave.Testing

[<Tests>]
let socketBinding (_ : SuaveConfig) =
  testList "SocketBinding" [
    testCase "on IPv6" <| fun _ ->
      let actual = (SocketBinding.create IPAddress.IPv6Any 3050us).ToString()
      let expected = "[::]:3050"
      Expect.equal actual expected "Should print as string"
    ]

[<Tests>]
let httpBinding (_ : SuaveConfig) =
  testList "HttpBinding" [
    testCase "assumption about IPV4 loopback" <| fun _ ->
      let actual = IPAddress.Loopback.ToString()
      Expect.equal actual "127.0.0.1" "Prints IPv4"

    testCase "assumption about IPV6 loopback" <| fun _ ->
      let actual = IPAddress.IPv6Loopback.ToString()
      Expect.equal actual "::1" "Prints IPv6"

    testCase "IPv6 uri" <| fun _ ->
      let binding = HttpBinding.create HTTP IPAddress.IPv6Loopback 8084us
      let actual = binding.uri "/a" "b=2" |> sprintf "%O"
      Expect.equal actual "http://[::1]:8084/a?b=2" "uri"

    testCase "IPv6 uri 2" <| fun _ ->
      let binding = HttpBinding.create HTTP IPAddress.IPv6Loopback 80us
      let actual = binding.uri "/a" "b=2" |> sprintf "%O"
      Expect.equal actual "http://[::1]/a?b=2" "uri"

    testCase "IPv4 uri" <| fun _ ->
      let binding = HttpBinding.create HTTP IPAddress.Loopback 8084us
      let actual = binding.uri "/a" "b=2" |> sprintf "%O"
      Expect.equal actual "http://127.0.0.1:8084/a?b=2" "uri"

    testCase "IPv4 uri 2" <| fun _ ->
      let binding = HttpBinding.create HTTP IPAddress.Loopback 80us
      let actual = binding.uri "/a" "b=2" |> sprintf "%O"
      Expect.equal actual "http://127.0.0.1/a?b=2" "uri"

    testCase "IPv4 uri 3" <| fun _ ->
      let binding = HttpBinding.create HTTP IPAddress.Loopback 80us
      let actual = binding.uri "/" "" |> sprintf "%O"
      Expect.equal actual "http://127.0.0.1/" "uri"

    testCase "IPv4 uri 4" <| fun _ ->
      let binding = HttpBinding.create HTTP IPAddress.Loopback 80us
      let actual = binding.uri "" "" |> sprintf "%O"
      Expect.equal actual "http://127.0.0.1/" "uri"
    ]


[<Tests>]
let httpReqIndexedPropertyFormData (_ : SuaveConfig) =

  let createReq (data : FormUrlEncodedContent) =
    {HttpRequest.empty with rawForm = data.ReadAsByteArrayAsync().Result}

  testList "Http Request Index Property for retrieving Form data" [
    testCase "get form value for the given key" <| fun _ ->
      use data = new FormUrlEncodedContent(dict [ "name", "bob"])
      let req = createReq data
      Expect.equal req.["name"] (Some "bob") "Should contain form data"
    testCase "get form value for a key which is absent" <| fun _ ->
      use data = new FormUrlEncodedContent(dict [ "name", "bob"])
      let req = createReq data
      Expect.equal req.["age"] None "form data "
  ]

[<Tests>]
let httpReqIndexedPropertyQueryStringData (_ : SuaveConfig) =

  let createReq rawQuery =
    {HttpRequest.empty with rawQuery = rawQuery}

  testList "Http Request Index Property for retrieving query string data" [
    testCase "get query string value for the given key" <| fun _ ->
      let req1 = createReq "name=bob"
      let req2 = createReq "name=bob&age=24"
      Expect.equal req1.["name"] (Some "bob") "query string data "
      Expect.equal req2.["age"] (Some "24") "query string data "
    testCase "get query string value for a key which is absent" <| fun _ ->
      let req1 = createReq ""
      let req2 = createReq "name=bob"
      Expect.equal req1.["age"] None "query string data "
      Expect.equal req2.["age"] None "query string data "
  ]

[<Tests>]
let httpReqIndexedPropertyMultiPartFieldsData (_ : SuaveConfig) =

  let createReq multiPartFields =
    {HttpRequest.empty with multiPartFields = multiPartFields}

  testList "Http Request Index Property for retrieving multi part fields data" [
    testCase "get multi part fields value for the given key" <| fun _ ->
      let req = createReq [("name", "bob")]
      Expect.equal req.["name"] (Some "bob") "multi part fields data "
    testCase "get multi part fields value for a key which is absent" <| fun _ ->
      let req = createReq [("name", "bob")]
      Expect.equal req.["age"] None "multi part fields data "
  ]
