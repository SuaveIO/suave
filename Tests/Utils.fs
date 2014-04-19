module Suave.Tests.Utils

open Suave.Utils.Compression
open Suave.Utils.Parsing

open Fuchu

open System.Text

[<Tests>]
let utilities =
  testList "trying some utility functions" [
    testCase "loopback ipv4" <| fun _ ->
      Assert.Equal("127.0.0.1 is a local address", true, is_local_address "127.0.0.1")

    testCase "loopback ipv6" <| fun _ ->
      Assert.Equal("::0 is a local address", true, is_local_address "::1")

    testProperty "gzip_encode/gzip_decode" <| fun str ->
      let get_bytes  = Encoding.UTF8.GetBytes  : string -> byte []
      let from_bytes = Encoding.UTF8.GetString : byte [] -> string
      Assert.Equal(
        "compress >> decompress == identity",
        str, (get_bytes >> gzip_encode >> gzip_decode >> from_bytes) str)
  ]
