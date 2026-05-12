module Suave.Tests.ServerKey

open Expecto

open Suave
open Suave.Utils
open Suave.Tests.TestUtilities

[<Tests>]
let serverKeyTest (cfg : SuaveConfig) =
  testList "server key" [
    testList "validate" [
      // No exception = successful test
      testCase "succeeds when the key has the correct number of bytes" <| fun _ ->
        let testKey : byte [] = Array.zeroCreate (int Crypto.KeyLength)
        ThreadSafeRandom.nextBytes testKey
        let key = ServerKey.validate testKey
        Assert.Equal ("The two keys should be the same", testKey, key)
      testCase "fails when the key is too short" <| fun _ ->
        try
          int (Crypto.KeyLength - 1us)
          |> (Array.zeroCreate >> ServerKey.validate >> ignore)
          failtest "A key smaller than the proper key length should have been invalid"
        with _ -> ()
      testCase "fails when the key is too long" <| fun _ ->
        try
          int (Crypto.KeyLength + 1us)
          |> (Array.zeroCreate >> ServerKey.validate >> ignore)
          failtest "A key larger than the proper key length should have been invalid"
        with _ -> ()
      ]
    testList "from base 64" [
      testCase "succeeds when the string is well-formed" <| fun _ ->
        let testKey : byte [] = Array.zeroCreate (int Crypto.KeyLength)
        ThreadSafeRandom.nextBytes testKey
        let key = ServerKey.fromBase64 (System.Convert.ToBase64String testKey)
        Assert.Equal ("The two keys should be the same", testKey, key)
      testCase "fails if key is wrong length" <| fun _ ->
        let testKey : byte [] = Array.zeroCreate (int Crypto.KeyLength - 1)
        ThreadSafeRandom.nextBytes testKey
        try
          ServerKey.fromBase64 (System.Convert.ToBase64String testKey) |> ignore
          failtest "The server key creation should have failed"
        with _ -> ()
      testCase "fails if the key is malformed" <| fun _ ->
        try
          ServerKey.fromBase64 "this isn't a base64-encoded string" |> ignore
          failtest "The base64 decoding should have failed"
        with _ -> ()
      ]
    ]

[<Tests>]
let startUpTest (cfg : SuaveConfig) =
  testList "startup validation" [
    testCase "startup fails when key is wrong length" <| fun _ ->
      try
        let badKey : byte [] = Array.zeroCreate 5
        startWebServerAsync { cfg with serverKey = badKey } (Successful.OK "") |> ignore
        failtest "Server startup should have failed for a key the wrong length"
      with _ -> ()
    ]
