﻿module Suave.Tests.Utils

open System
open System.Text

open Suave
open Suave.Utils
open Suave.Utils.Encoding
open Suave.Utils.Parsing

open Fuchu

open Suave.Types
open TestUtilities

#nowarn "25"

[<Tests>]
let utilities (_: SuaveConfig) =
  testList "trying some utility functions" [
    testCase "loopback ipv4" <| fun _ ->
      Assert.Equal("127.0.0.1 is a local address", true, isLocalAddress "127.0.0.1")

    testCase "loopback ipv6" <| fun _ ->
      Assert.Equal("::0 is a local address", true, isLocalAddress "::1")

    testPropertyWithConfig fsCheckConfig "gzip_encode/gzip_decode" <| fun str ->
      let get_bytes  = Encoding.UTF8.GetBytes  : string -> byte []
      let from_bytes = Encoding.UTF8.GetString : byte [] -> string
      Assert.Equal(
        "compress >> decompress == identity",
        str, (get_bytes >> gzipEncode >> gzipDecode >> from_bytes) str)

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
                     String.eqConstTime str1 str2)

    /// is 0.078125 KiB long (and that *1.375 when base-64 encoded)
    testCase "crypto hello world" <| fun _ ->
      let str = "Hello World"
      let ca (str : string) = str.ToCharArray() |> Array.map (string<<int) |> String.concat ","
      let key = Crypto.generateStdKey ()
      let (Choice1Of2 cipher) = Crypto.secretboxOfText key str
      let (Choice1Of2 plain) = Crypto.secretboxOpenAsString key cipher
      Assert.Equal(sprintf "'%s':%s = D(k, E(k, '%s':%s))" plain (ca plain) str (ca str), str, plain)

    testCase "crypto hello world - avoid padding oracle" <| fun _ ->
      let str = "Hello World"
      let ca (str : string) = str.ToCharArray() |> Array.map (string<<int) |> String.concat ","
      let key = Crypto.generateStdKey ()
      let (Choice1Of2 cipher) = Crypto.secretboxOfText key str
      cipher.[cipher.Length - Crypto.HMACLength - 1] <- 0uy
      cipher.[cipher.Length - Crypto.HMACLength - 2] <- 0uy
      cipher.[cipher.Length - Crypto.HMACLength - 3] <- 0uy
      cipher.[cipher.Length - Crypto.HMACLength - 4] <- 0uy
      cipher.[cipher.Length - Crypto.HMACLength - 5] <- 0uy
      cipher.[cipher.Length - Crypto.HMACLength - 6] <- 0uy
      cipher.[cipher.Length - Crypto.HMACLength - 7] <- 0uy
      cipher.[cipher.Length - Crypto.HMACLength - 8] <- 0uy
      match Crypto.secretboxOpen key cipher with
      | Choice1Of2 _ -> Tests.failtest "should not decrypt successfully"
      | Choice2Of2 (Crypto.AlteredOrCorruptMessage(msg) as aocm) -> ()
      | x -> Tests.failtestf "got %A" x

    testPropertyWithConfig fsCheckConfig "can encrypt and then decrypt any string" <| fun (str : string) ->
      let ca (str : string) = str.ToCharArray() |> Array.map (string<<int) |> String.concat ","
      if not <| String.IsNullOrWhiteSpace str then
        let key = Crypto.generateStdKey ()
        let (Choice1Of2 cipher) = Crypto.secretboxOfText key str
        let (Choice1Of2 plain) = Crypto.secretboxOpenAsString key cipher
        Assert.Equal(sprintf "'%s':%s = D(k, E(k, '%s':%s))" plain (ca plain) str (ca str), str, plain)

    testPropertyWithConfig fsCheckConfig "Bytes.encode_safe_base64 encoded <-> decoded" <| fun (str : string) ->
      let enc, dec = Bytes.cookieEncoding
      Assert.Equal("roundtrip", str, Encoding.UTF8.GetString (dec (enc (Encoding.UTF8.GetBytes str))))
  ]
