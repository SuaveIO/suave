module Suave.Tests.Utils

open System
open System.Text

open Suave
open Suave.Utils
open Suave.Utils.Compression
open Suave.Utils.Parsing

open Expecto
open TestUtilities

#nowarn "25"

[<Tests>]
let utilities (_: SuaveConfig) =
  testList "trying some utility functions" [
    testCase "loopback ipv4" <| fun _ ->
      Expect.isTrue (isLocalAddress "127.0.0.1") "127.0.0.1 is a local address"

    testCase "loopback ipv6" <| fun _ ->
      Expect.isTrue (isLocalAddress "::1") "::1 is a local address"

    testPropertyWithConfig fsCheckConfig "gzipEncode/gzipDecode" <| fun str ->
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
                     String.equalsConstantTime str1 str2)

    /// is 0.078125 KiB long (and that *1.375 when base-64 encoded)
    testCase "crypto hello world" <| fun _ ->
      let str = "Hello World"
      let ca (str : string) = str.ToCharArray() |> Array.map (string<<int) |> String.concat ","
      let key = Crypto.generateStdKey ()
      let (Choice1Of2 cipher) = Crypto.secretboxOfText key str
      let (Choice1Of2 plain) = Crypto.secretboxOpenAsString key cipher
      Expect.equal plain str (sprintf "'%s':%s = D(k, E(k, '%s':%s))" plain (ca plain) str (ca str))

    testCase "crypto hello world - avoid padding oracle" <| fun _ ->
      let str = "Hello World"
      let ca (str : string) = str.ToCharArray() |> Array.map (string<<int) |> String.concat ","
      let key = Crypto.generateStdKey ()
      let (Choice1Of2 cipher) = Crypto.secretboxOfText key str
      let dataFromEnd negOffset =
        int (uint16 cipher.Length - Crypto.HMACLength - negOffset)
      cipher.[dataFromEnd 1us] <- 0uy
      cipher.[dataFromEnd 2us] <- 0uy
      cipher.[dataFromEnd 3us] <- 0uy
      cipher.[dataFromEnd 4us] <- 0uy
      cipher.[dataFromEnd 5us] <- 0uy
      cipher.[dataFromEnd 6us] <- 0uy
      cipher.[dataFromEnd 7us] <- 0uy
      cipher.[dataFromEnd 8us] <- 0uy
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
        Expect.equal plain str (sprintf "'%s':%s = D(k, E(k, '%s':%s))" plain (ca plain) str (ca str))

    testPropertyWithConfig fsCheckConfig "Bytes.encode_safe_base64 encoded <-> decoded" <| fun (str : string) ->
      let enc, dec = Bytes.cookieEncoding
      Expect.equal (Encoding.UTF8.GetString (dec (enc (Encoding.UTF8.GetBytes str)))) str "roundtrip"
  ]
