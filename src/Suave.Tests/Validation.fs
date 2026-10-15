module Suave.Tests.Validation

open System.Text.Json
open Expecto
open Suave
open Suave.Validation
open Suave.ValidationWebParts

let private expectRequired name (validator: Validator<string, string>) =
  testCase (name + " returns Invalid on null") <| fun _ ->
    match validator null with
    | Invalid [ e ] ->
      Expect.equal e.field "code" "error should name the field"
      Expect.equal e.message "Value is required" "error should say the value is required"
    | other -> failtestf "expected a single required error, got %A" other

[<Tests>]
let stringValidatorNullTests (_ : SuaveConfig) =
  testList "string validators on null input" [
    expectRequired "minLength" (String.minLength 2 "code")
    expectRequired "maxLength" (String.maxLength 2 "code")
    expectRequired "exactLength" (String.exactLength 2 "code")
    expectRequired "lengthBetween" (String.lengthBetween 1 5 "code")
    expectRequired "matches" (String.matches "^a$" "code" "must be a")
    expectRequired "email" (String.email "code")
    expectRequired "alphanumeric" (String.alphanumeric "code")
    expectRequired "alpha" (String.alpha "code")
    expectRequired "numeric" (String.numeric "code")
    expectRequired "contains" (String.contains "x" "code")
    expectRequired "startsWith" (String.startsWith "x" "code")
    expectRequired "endsWith" (String.endsWith "x" "code")

    testCase "url returns Invalid on null" <| fun _ ->
      match String.url "code" null with
      | Invalid [ _ ] -> ()
      | other -> failtestf "expected a single error, got %A" other

    testCase "notEmpty is unchanged on null" <| fun _ ->
      match String.notEmpty "code" null with
      | Invalid [ e ] -> Expect.equal e.message "Value cannot be empty" "notEmpty message"
      | other -> failtestf "expected a single error, got %A" other

    testCase "non-null input still validates" <| fun _ ->
      Expect.equal (String.minLength 2 "code" "abc") (Valid "abc") "valid input passes"
      match String.minLength 5 "code" "abc" with
      | Invalid [ e ] -> Expect.equal e.message "Value must be at least 5 characters" "length message"
      | other -> failtestf "expected a single error, got %A" other
  ]

[<Tests>]
let andAlsoTests (_ : SuaveConfig) =
  let a = fail (errorSimple "a" "first")
  let b = fail (errorSimple "b" "second")
  testList "andAlso" [
    testCase "operator reports errors left to right" <| fun _ ->
      match (a <++> b) () with
      | Invalid errs -> Expect.equal (errs |> List.map (fun e -> e.field)) [ "a"; "b" ] "left operand first"
      | other -> failtestf "expected errors, got %A" other

    testCase "piped form runs the piped validator first" <| fun _ ->
      match (a |> andAlso b) () with
      | Invalid errs -> Expect.equal (errs |> List.map (fun e -> e.field)) [ "a"; "b" ] "piped operand first"
      | other -> failtestf "expected errors, got %A" other
  ]

[<Tests>]
let validationErrorsToJsonTests (_ : SuaveConfig) =
  let parse (json: string) =
    use doc = JsonDocument.Parse json
    doc.RootElement.GetProperty("errors").EnumerateArray()
    |> Seq.map (fun e ->
      let str (name: string) =
        match e.TryGetProperty name with
        | true, v -> Some (v.GetString())
        | _ -> None
      str "code", str "field", str "message")
    |> List.ofSeq

  testList "validationErrorsToJson" [
    testCase "escapes quotes, backslashes and control characters" <| fun _ ->
      let message = "Value must contain '\"' or \\ \n\t and </script>"
      let field = "na\"me\\"
      let json = validationErrorsToJson [ error field message (Some "c\"ode") ]
      Expect.equal (parse json) [ Some "c\"ode", Some field, Some message ] "round-trips through a JSON parser"

    testCase "omits code when None" <| fun _ ->
      let json = validationErrorsToJson [ errorSimple "name" "required"; error "age" "too low" (Some "min") ]
      Expect.equal (parse json) [ None, Some "name", Some "required"; Some "min", Some "age", Some "too low" ] "same shape"
      Expect.isFalse (json.Contains "\"code\":null") "code is not written as null"

    testCase "empty list" <| fun _ ->
      Expect.equal (validationErrorsToJson []) """{"errors":[]}""" "empty errors array"

    testCase "contains message stays valid JSON" <| fun _ ->
      match String.contains "\"" "q" "abc" with
      | Invalid errs -> Expect.equal (parse (validationErrorsToJson errs)) [ None, Some "q", Some "Value must contain '\"'" ] "parses"
      | other -> failtestf "expected errors, got %A" other
  ]