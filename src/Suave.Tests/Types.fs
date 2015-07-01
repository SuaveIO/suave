module Suave.Tests.Types

open System
open System.Net

open Fuchu

open Suave.Sockets
open Suave.Types
open System.Net.Http
open Suave.Testing

[<Tests>]
let socketBinding =
  testList "SocketBinding" [
    testCase "on IPv6" <| fun _ ->
      Assert.Equal("", "[::]:3050", (SocketBinding.mk IPAddress.IPv6Any 3050us).ToString())
    ]

[<Tests>]
let httpBinding =
  testList "HttpBinding" [
    testCase "assumption about IPV4 loopback" <| fun _ ->
      Assert.Equal("", "127.0.0.1", IPAddress.Loopback.ToString())

    testCase "assumption about IPV6 loopback" <| fun _ ->
      Assert.Equal("", "::1", IPAddress.IPv6Loopback.ToString())

    testCase "IPv6 uri" <| fun _ ->
      let binding = HttpBinding.mk HTTP IPAddress.IPv6Loopback 8084us
      Assert.Equal("uri", "http://[::1]:8084/a?b=2",
                   binding.uri "/a" "b=2" |> sprintf "%O")

    testCase "IPv6 uri 2" <| fun _ ->
      let binding = HttpBinding.mk HTTP IPAddress.IPv6Loopback 80us
      Assert.Equal("uri", "http://[::1]/a?b=2",
                   binding.uri "/a" "b=2" |> sprintf "%O")

    testCase "IPv4 uri" <| fun _ ->
      let binding = HttpBinding.mk HTTP IPAddress.Loopback 8084us
      Assert.Equal("uri", "http://127.0.0.1:8084/a?b=2",
                   binding.uri "/a" "b=2" |> sprintf "%O")

    testCase "IPv4 uri 2" <| fun _ ->
      let binding = HttpBinding.mk HTTP IPAddress.Loopback 80us
      Assert.Equal("uri", "http://127.0.0.1/a?b=2",
                   binding.uri "/a" "b=2" |> sprintf "%O")

    testCase "IPv4 uri 3" <| fun _ ->
      let binding = HttpBinding.mk HTTP IPAddress.Loopback 80us
      Assert.Equal("uri", "http://127.0.0.1/",
                   binding.uri "/" "" |> sprintf "%O")
    ]


[<Tests>]
let httpReqIndexedPropertyFormData =

  let createReq (data : FormUrlEncodedContent) = 
    {HttpRequest.empty with rawForm = data.ReadAsByteArrayAsync().Result}

  testList "Http Request Index Property for retrieving Form data" [
    testCase "get form value for the given key" <| fun _ ->
      use data = new FormUrlEncodedContent(dict [ "name", "bob"])
      let req = createReq data
      Assert.Equal("form data ", Some "bob", req.["name"])
    testCase "get form value for a key which is absent" <| fun _ ->
      use data = new FormUrlEncodedContent(dict [ "name", "bob"])
      let req = createReq data
      Assert.Equal("form data ", None, req.["age"])
  ]
  
[<Tests>]
let httpReqIndexedPropertyQueryStringData =

  let createReq rawQuery = 
    {HttpRequest.empty with rawQuery = rawQuery}

  testList "Http Request Index Property for retrieving query string data" [
    testCase "get query string value for the given key" <| fun _ ->
      let req1 = createReq "name=bob"
      let req2 = createReq "name=bob&age=24"
      Assert.Equal("query string data ", Some "bob", req1.["name"])
      Assert.Equal("query string data ", Some "24", req2.["age"])
    testCase "get query string value for a key which is absent" <| fun _ ->
      let req1 = createReq ""
      let req2 = createReq "name=bob"
      Assert.Equal("query string data ", None, req1.["age"])
      Assert.Equal("query string data ", None, req2.["age"])
  ]

[<Tests>]
let httpReqIndexedPropertyMultiPartFieldsData =

  let createReq multiPartFields = 
    {HttpRequest.empty with multiPartFields = multiPartFields}

  testList "Http Request Index Property for retrieving multi part fields data" [
    testCase "get multi part fields value for the given key" <| fun _ ->
      let req = createReq [("name", "bob")]
      Assert.Equal("multi part fields data ", Some "bob", req.["name"])
    testCase "get multi part fields value for a key which is absent" <| fun _ ->
      let req = createReq [("name", "bob")]
      Assert.Equal("multi part fields data ", None, req.["age"])
  ]