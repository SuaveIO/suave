module Suave.Tests.HttpFile

open Fuchu

open System
open System.IO
open System.Text

open Suave.Types
open Suave.Http
open Suave.Http.Successful
open Suave.Tests.TestUtilities

[<Tests>]
let ``canonicalization attacks`` =
  testList "canonicalization attacks" [
    testCase "should throw" <| fun _ ->
      Assert.Raise("'../../passwd' is not a valid path",
        typeof<Exception>,
        fun _ -> Files.resolve_path current_path "../../passwd" |> ignore)
  ]

[<Tests>]
let compression =
  let run_with' = run_with default_config

  let test_file_size = (new FileInfo(Path.Combine(current_path,"test-text-file.txt"))).Length

  testList "getting basic gzip/deflate responses" [
      testCase "200 OK returns 'Havana' with gzip " <| fun _ ->
        Assert.Equal("expecting 'Havana'", "Havana", run_with' (OK "Havana") |> req_gzip HttpMethod.GET "/" None)

      testCase "200 OK returns 'Havana' with deflate " <| fun _ ->
        Assert.Equal("expecting 'Havana'", "Havana", run_with' (OK "Havana") |> req_deflate HttpMethod.GET "/" None)

      testCase "verifiying we get the same size uncompressed" <| fun _ ->
        Assert.Equal("length should match"
        , test_file_size
        , (run_with' (Files.browse_file' "test-text-file.txt") |> req_bytes HttpMethod.GET "/" None).Length |> int64)

      testCase "gzip static file" <| fun _ ->
        Assert.Equal("length should match"
        , test_file_size
        , (run_with' (Files.browse_file' "test-text-file.txt") |> req_gzip_bytes HttpMethod.GET "/" None).Length |> int64)
    ]
