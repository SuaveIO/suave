module Suave.Tests.Sscanf

open Expecto
open TestUtilities
open Suave
open Suave.Sscanf

[<Tests>]
let scanTests (_ : SuaveConfig) =
  testList "when scanning" [
    testCase "with escaped % before escaped placeholder in string" <| fun _ ->
      let result = sscanf "(%%%s,%M)" "(%hello, 4.53)"
      Expect.equal result ("hello", 4.53m) "should match correctly"

    // covers issue: suave/issues/74
    testCase "with escaped % before unescaped placeholder char in string" <| fun _ ->
      let result = sscanf "(%%const%d)" "(%const5)"
      Expect.equal result 5 "should match correctly"

    testCase "with escaped % before non-placeholder char in string" <| fun _ ->
      let result = sscanf "(%%hello%s,%M)" "(%helloWorld, 4.53)"
      Expect.equal result ("World", 4.53m) "should match correctly"

    testCase "with dashed string parts" <| fun _ ->
      let result = sscanf "%s-%s-%s" "test-this-string"
      Expect.equal result ("test", "this", "string") "should match each string correctly"

    testCase "with mixed datatypes" <| fun _ ->
      let result = sscanf "%b-%d-%i,%u,%x,%X,%o" "false-42--31,13,ff,FF,42"
      Expect.equal result (false, 42, -31, 13u, 0xff, 0xFF, 0o42)
                   "should parse each item correctly"

    testCase "with mixed, space separated datatypes" <| fun _ ->
      let result = sscanf "%f %F %g %G %e %E %c" "1 2.1 3.4 .3 43.2e32 0 f"
      Expect.equal result (1., 2.1, 3.4, 0.3, 43.2e+32, 0., 'f')
                   "should parse each item correctly"

    testCase "with int, int64 and uint32 " <| fun _ ->
      let result = sscanf "%d %d %u" "100 9223372036854775807 42"
      Expect.equal (100, 9223372036854775807L, 42u) result "should parse each item correctly"
    ]

open Suave.Http
open Suave.Filters
open Suave.Successful
open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let moreScanTests cfg =
  let runWithConfig = runWith cfg
  testList "when scanning via web server" [
    testCase "parse a long integer" <| fun _ ->
      Assert.Equal("returns 9223372036854775807",
                   "9223372036854775807",
                   runWithConfig (pathScan "/%d" (fun (a:int64) -> OK(string a)))
                   |> req HttpMethod.GET "/9223372036854775807" None)
    ]
