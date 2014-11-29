module Suave.Tests.Utils

open Suave
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

    testCase "str eql" <| fun _ ->
      for (str1, str2) in
        [
          "", ""
          "a", "b"
          "a", "aa"
          "aa", "aa"
        ] do
        Assert.Equal("should have same as normal str equal",
                     str1.Equals str2,
                     String.cnst_time_cmp_ord str1 str2)
  ]
