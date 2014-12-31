module Suave.Tests.Sscanf

open Fuchu

open Suave.Sscanf

[<Tests>]
let scan_tests =
    testList "when scanning " [
        testCase "with escaped % before escaped placeholder in string" <| fun _ ->
            let result = sscanf "(%%%s,%M)" "(%hello, 4.53)"
            Assert.Equal("should match correctly", ("hello", 4.53m), result)

        // covers issue: suave/issues/74
        testCase "with escaped % before unescaped placeholder char in string" <| fun _ ->
            let result = sscanf "(%%const%d)" "(%const5)"
            Assert.Equal("should match correctly", (5), result)

        testCase "with escaped % before non-placeholder char in string" <| fun _ ->
            let result = sscanf "(%%hello%s,%M)" "(%helloWorld, 4.53)"
            Assert.Equal("should match correctly", ("World", 4.53m), result)

        testCase "with dashed string parts" <| fun _ ->
            let result = sscanf "%s-%s-%s" "test-this-string"
            Assert.Equal("should match each string correctly", ("test", "this", "string"), result)

        testCase "with mixed datatypes" <| fun _ ->
            let result = sscanf "%b-%d-%i,%u,%x,%X,%o" "false-42--31,13,ff,FF,42"
            Assert.Equal("should parse each item correctly", (false, 42, -31, 13, 0xff, 0xFF, 0o42), result)

        testCase "with mixed, space separated datatypes" <| fun _ ->
            let result = sscanf "%f %F %g %G %e %E %c" "1 2.1 3.4 .3 43.2e32 0 f"
            Assert.Equal("should parse each item correctly", (1., 2.1, 3.4, 0.3, 43.2e+32, 0., 'f'), result)

        testCase "with int, int64 and uint32 " <| fun _ ->
            let result = sscanf "%d %d %u" "100 9223372036854775807 42"
            Assert.Equal("should parse each item correctly", (100, 9223372036854775807L, 42u), result)
        ]

open Suave.Types
open Suave.Types.Methods
open Suave.Http.Applicatives
open Suave.Http.Successful
open Suave.Tests.TestUtilities

[<Tests>]
let more_scan_tests =
  let run_with' = run_with default_config
  testList "when scanning via web server" [
    testCase "parse a long integer" <| fun _ ->
      Assert.Equal("returns 9223372036854775807", "9223372036854775807", 
        run_with' (url_scan "/%d" (fun (a:int64) -> OK(string a))) |> req HttpMethod.GET "/9223372036854775807" None) 
    ]


