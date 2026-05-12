module SecurityHeadersExample.Program

open Suave
open Suave.Filters
open Suave.Operators
open Suave.Successful
open Suave.SecurityHeaders

let balancedApp =
    securityHeaders balancedConfig >=> OK """
    <html>
    <head><title>Balanced Security</title></head>
    <body>
        <h1>Balanced Security Headers</h1>
        <p>Check the response headers in your browser's DevTools!</p>
    </body>
    </html>
    """

let strictApp =
    securityHeaders strictConfig >=> OK """
    <html>
    <head><title>Strict Security</title></head>
    <body>
        <h1>Strict Security Headers</h1>
        <p>Maximum security configuration.</p>
    </body>
    </html>
    """

let relaxedApp =
    securityHeaders relaxedConfig >=> OK """
    <html>
    <head><title>Relaxed Security</title></head>
    <body>
        <h1>Relaxed Security Headers</h1>
        <p>Development mode configuration.</p>
    </body>
    </html>
    """

let customConfig =
    emptyConfig
    |> withCSPRule CSPDirective.DefaultSrc [CSPSource.Self]
    |> withCSPRule CSPDirective.ScriptSrc [CSPSource.Self]
    |> withHSTS 31536000 true false
    |> withFrameOptions FrameOptions.SameOrigin

let customApp =
    securityHeaders customConfig >=> OK """
    <html>
    <head><title>Custom Security</title></head>
    <body>
        <h1>Custom Security Configuration</h1>
        <p>Custom security headers applied.</p>
    </body>
    </html>
    """

let nonceApp (ctx: HttpContext) =
    let nonce = generateNonce()
    let nonceConfig =
        emptyConfig
        |> withCSPRule CSPDirective.ScriptSrc [CSPSource.Self; CSPSource.Nonce nonce]
    
    let html = sprintf """<html><body><h1>CSP with Nonce</h1><p>Nonce: %s</p><script nonce="%s">document.body.style.backgroundColor='#f0f0f0';</script></body></html>""" nonce nonce
    
    (securityHeaders nonceConfig >=> OK html) ctx

let app =
    choose [
        GET >=> choose [
            path "/" >=> balancedApp
            path "/strict" >=> strictApp
            path "/relaxed" >=> relaxedApp
            path "/custom" >=> customApp
            path "/csp-nonce" >=> nonceApp
        ]
    ]

[<EntryPoint>]
let main argv =
    printfn "Suave Security Headers Example"
    printfn "Server: http://localhost:8080"
    
    let config = { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 8080 ] }
    startWebServer config app
    0
