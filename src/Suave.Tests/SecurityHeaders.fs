module Suave.Tests.SecurityHeaders

open System
open System.Net
open System.Net.Http
open Suave
open Suave.Operators
open Suave.Successful
open Suave.SecurityHeaders
open Suave.Tests.TestUtilities
open Suave.Testing
open Expecto

let getHeader (response: HttpResponseMessage) (name: string) =
  match response.Headers.TryGetValues(name) with
  | true, values -> Some (Seq.head values)
  | false, _ -> None

[<Tests>]
let securityHeadersTests cfg =
  let runWithConfig = runWith cfg
  
  testList "security headers" [
    testCase "empty config no headers" <| fun _ ->
      let app = securityHeaders emptyConfig >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        Expect.isNone (getHeader res "Content-Security-Policy") "no CSP"
        Expect.isNone (getHeader res "X-Frame-Options") "no frame opts"
      ) ctx
    
    testCase "balanced config adds headers" <| fun _ ->
      let app = securityHeaders balancedConfig >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        Expect.isSome (getHeader res "Content-Security-Policy") "CSP set"
        Expect.isSome (getHeader res "X-Frame-Options") "frame opts set"
      ) ctx
    
    testCase "CSP with directives" <| fun _ ->
      let config = 
        emptyConfig
        |> withCSPRule CSPDirective.DefaultSrc [CSPSource.Self]
        |> withCSPRule CSPDirective.ScriptSrc [CSPSource.Self]
      
      let app = securityHeaders config >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        match getHeader res "Content-Security-Policy" with
        | Some csp ->
            Expect.stringContains csp "default-src 'self'" "has default-src"
            Expect.stringContains csp "script-src 'self'" "has script-src"
        | None -> failwith "CSP missing"
      ) ctx
    
    testCase "CSP report-only" <| fun _ ->
      let config = 
        emptyConfig
        |> withCSPRule CSPDirective.DefaultSrc [CSPSource.Self]
        |> withCSPReportOnly true
      
      let app = securityHeaders config >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        Expect.isSome (getHeader res "Content-Security-Policy-Report-Only") "report-only set"
        Expect.isNone (getHeader res "Content-Security-Policy") "regular CSP not set"
      ) ctx
    
    testCase "CSP Level 3 strict-dynamic" <| fun _ ->
      let config = 
        emptyConfig
        |> withCSPRule CSPDirective.ScriptSrc [CSPSource.StrictDynamic; CSPSource.Nonce "test"]
      
      let app = securityHeaders config >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        match getHeader res "Content-Security-Policy" with
        | Some csp -> Expect.stringContains csp "'strict-dynamic'" "has strict-dynamic"
        | None -> failwith "CSP missing"
      ) ctx
    
    testCase "HSTS config" <| fun _ ->
      let config = emptyConfig |> withHSTS 31536000 true false
      let app = securityHeaders config >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        match getHeader res "Strict-Transport-Security" with
        | Some hsts ->
            Expect.stringContains hsts "max-age=31536000" "has max-age"
            Expect.stringContains hsts "includeSubDomains" "has includeSubDomains"
        | None -> failwith "HSTS missing"
      ) ctx
    
    testCase "X-Frame-Options DENY" <| fun _ ->
      let config = emptyConfig |> withFrameOptions FrameOptions.Deny
      let app = securityHeaders config >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        match getHeader res "X-Frame-Options" with
        | Some v -> Expect.equal v "DENY" "is DENY"
        | None -> failwith "frame opts missing"
      ) ctx
    
    testCase "Referrer-Policy" <| fun _ ->
      let config = emptyConfig |> withReferrerPolicy ReferrerPolicy.NoReferrer
      let app = securityHeaders config >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        match getHeader res "Referrer-Policy" with
        | Some v -> Expect.equal v "no-referrer" "is no-referrer"
        | None -> failwith "referrer policy missing"
      ) ctx
    
    testCase "Permissions-Policy" <| fun _ ->
      let config = 
        emptyConfig
        |> withPermissionsRule PermissionsDirective.Camera PermissionsAllowlist.NoOrigins
      
      let app = securityHeaders config >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        match getHeader res "Permissions-Policy" with
        | Some v -> Expect.stringContains v "camera=()" "has camera"
        | None -> failwith "permissions policy missing"
      ) ctx
    
    testCase "X-Content-Type-Options" <| fun _ ->
      let config = { emptyConfig with xContentTypeOptions = true }
      let app = securityHeaders config >=> OK "test"
      let ctx = runWithConfig app
      
      reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None id (fun res ->
        match getHeader res "X-Content-Type-Options" with
        | Some v -> Expect.equal v "nosniff" "is nosniff"
        | None -> failwith "content type opts missing"
      ) ctx
    
    testCase "generateNonce" <| fun _ ->
      let nonce1 = generateNonce()
      let nonce2 = generateNonce()
      
      Expect.isGreaterThan (String.length nonce1) 0 "nonce not empty"
      Expect.notEqual nonce1 nonce2 "nonces unique"
      
      // Valid base64
      try System.Convert.FromBase64String(nonce1) |> ignore
      with _ -> failwith "invalid base64"
  ]
