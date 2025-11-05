# Suave Security Headers Example

This example demonstrates the comprehensive Security Headers module for Suave, which provides type-safe configuration of HTTP security headers including Content Security Policy (CSP) Level 3, HSTS, and more.

## Features

The SecurityHeaders module provides:

- **Content Security Policy (CSP) Level 3** with support for:
  - All CSP directives (script-src, style-src, img-src, etc.)
  - Modern features: `strict-dynamic`, `report-to`, trusted types
  - Nonces and hashes for inline scripts/styles
  - Report-only mode for testing
  
- **HTTP Strict Transport Security (HSTS)**
  - Configurable max-age, includeSubDomains, preload
  
- **X-Frame-Options**
  - DENY, SAMEORIGIN, or ALLOW-FROM configurations
  
- **Referrer-Policy**
  - All modern referrer policy values
  
- **Permissions-Policy** (formerly Feature-Policy)
  - Control browser features like camera, microphone, geolocation, etc.
  
- **Other Security Headers**
  - X-Content-Type-Options: nosniff
  - X-XSS-Protection
  - X-Permitted-Cross-Domain-Policies
  - Cache-Control for security

## Running the Example

```bash
cd examples/SecurityHeadersExample
dotnet run
```

Then visit:
- http://localhost:8080/ - Balanced security configuration
- http://localhost:8080/strict - Strict security configuration
- http://localhost:8080/relaxed - Relaxed security configuration
- http://localhost:8080/custom - Custom security configuration
- http://localhost:8080/csp-nonce - CSP with dynamic nonce

## Usage Examples

### 1. Using Preset Configurations

```fsharp
open Suave
open Suave.SecurityHeaders

// Balanced configuration (recommended for most apps)
let app = securityHeaders balancedConfig >=> OK "Hello, World!"

// Strict configuration (maximum security)
let app = securityHeaders strictConfig >=> OK "Hello, World!"

// Relaxed configuration (for development)
let app = securityHeaders relaxedConfig >=> OK "Hello, World!"
```

### 2. Building Custom Configuration

```fsharp
open Suave.SecurityHeaders

let customConfig =
    emptyConfig
    // Content Security Policy
    |> withCSPRule CSPDirective.DefaultSrc [CSPSource.Self]
    |> withCSPRule CSPDirective.ScriptSrc [
        CSPSource.Self
        CSPSource.Host "cdn.example.com"
        CSPSource.StrictDynamic
    ]
    |> withCSPRule CSPDirective.StyleSrc [
        CSPSource.Self
        CSPSource.UnsafeInline
    ]
    |> withCSPRule CSPDirective.ImgSrc [
        CSPSource.Self
        CSPSource.Scheme "data"
        CSPSource.Scheme "https"
    ]
    // HSTS
    |> withHSTS 31536000 true false  // 1 year, includeSubDomains, no preload
    // Frame options
    |> withFrameOptions FrameOptions.SameOrigin
    // Referrer policy
    |> withReferrerPolicy ReferrerPolicy.StrictOriginWhenCrossOrigin
    // Permissions policy
    |> withPermissionsRule PermissionsDirective.Camera PermissionsAllowlist.SelfOnly
    |> withPermissionsRule PermissionsDirective.Geolocation PermissionsAllowlist.NoOrigins

let app = securityHeaders customConfig >=> OK "Secure!"
```

### 3. CSP with Dynamic Nonce

```fsharp
open Suave.SecurityHeaders

let nonceApp (ctx: HttpContext) =
    // Generate a unique nonce for this request
    let nonce = generateNonce()
    
    let config =
        emptyConfig
        |> withCSPRule CSPDirective.ScriptSrc [
            CSPSource.Self
            CSPSource.Nonce nonce
        ]
    
    let html = sprintf """
        <html>
        <body>
            <script nonce="%s">
                // This inline script is allowed!
                console.log('Hello from inline script');
            </script>
        </body>
        </html>
    """ nonce
    
    (securityHeaders config >=> OK html) ctx
```

### 4. CSP Report-Only Mode

```fsharp
let config =
    emptyConfig
    |> withCSPRule CSPDirective.DefaultSrc [CSPSource.Self]
    |> withCSPReportOnly true  // Report violations but don't block

let app = securityHeaders config >=> OK "Testing CSP..."
```

### 5. Fine-grained Permissions Policy

```fsharp
let config =
    emptyConfig
    |> withPermissionsRule PermissionsDirective.Camera PermissionsAllowlist.NoOrigins
    |> withPermissionsRule PermissionsDirective.Microphone (
        PermissionsAllowlist.Origins ["https://trusted.example.com"]
    )
    |> withPermissionsRule PermissionsDirective.Geolocation PermissionsAllowlist.SelfOnly
    |> withPermissionsRule PermissionsDirective.Payment PermissionsAllowlist.AllOrigins

let app = securityHeaders config >=> OK "Permissions configured!"
```

## CSP Level 3 Features

The module includes full support for Content Security Policy Level 3:

### Script Directives

```fsharp
// Separate script element and attribute directives
|> withCSPRule CSPDirective.ScriptSrcElem [CSPSource.Self]
|> withCSPRule CSPDirective.ScriptSrcAttr [CSPSource.None]
```

### Style Directives

```fsharp
// Separate style element and attribute directives
|> withCSPRule CSPDirective.StyleSrcElem [CSPSource.Self]
|> withCSPRule CSPDirective.StyleSrcAttr [CSPSource.UnsafeInline]
```

### Modern Directives

```fsharp
// Worker sources
|> withCSPRule CSPDirective.WorkerSrc [CSPSource.Self]

// Navigation control
|> withCSPRule CSPDirective.NavigateTo [CSPSource.Self]

// Trusted Types (for XSS protection)
|> withCSPRule CSPDirective.RequireTrustedTypesFor []
|> withCSPRule CSPDirective.TrustedTypes []

// Reporting (use report-to instead of deprecated report-uri)
|> withCSPRule CSPDirective.ReportTo []
```

### Dynamic Script Loading

```fsharp
// Use strict-dynamic for dynamic script loading
|> withCSPRule CSPDirective.ScriptSrc [
    CSPSource.Nonce "abc123"
    CSPSource.StrictDynamic
]
```

## Type Safety

The module provides full type safety for all security header configurations:

```fsharp
// CSP Directives are strongly typed
type CSPDirective =
    | DefaultSrc | ScriptSrc | ScriptSrcElem | ScriptSrcAttr
    | StyleSrc | StyleSrcElem | StyleSrcAttr
    | ImgSrc | FontSrc | ConnectSrc | MediaSrc
    | ObjectSrc | FrameSrc | ChildSrc | FrameAncestors
    | FormAction | BaseUri | ManifestSrc | WorkerSrc
    | NavigateTo | ReportTo
    | RequireTrustedTypesFor | TrustedTypes
    | UpgradeInsecureRequests | BlockAllMixedContent

// CSP Sources are strongly typed
type CSPSource =
    | Self | None | UnsafeInline | UnsafeEval
    | UnsafeHashes | StrictDynamic | ReportSample
    | Nonce of string
    | Hash of algorithm: string * value: string
    | Scheme of string
    | Host of string
    | Custom of string

// All other headers have similar type-safe configurations
```

## Security Best Practices

1. **Start with `balancedConfig`** for most applications
2. **Use `strictConfig`** for maximum security (may break some features)
3. **Use `relaxedConfig`** only during development
4. **Use nonces** for inline scripts instead of `unsafe-inline`
5. **Enable HSTS** with a reasonable max-age
6. **Set X-Frame-Options** to prevent clickjacking
7. **Configure Permissions-Policy** to limit browser features
8. **Use CSP report-only mode** first to test your policy
9. **Avoid deprecated `report-uri`** - the module uses modern `report-to`

## Testing Your Configuration

Use your browser's Developer Tools to inspect the security headers:

1. Open DevTools (F12)
2. Go to the Network tab
3. Click on any request
4. Look at the Response Headers section

You should see headers like:
```
Content-Security-Policy: default-src 'self'; script-src 'self'...
Strict-Transport-Security: max-age=31536000; includeSubDomains
X-Frame-Options: SAMEORIGIN
X-Content-Type-Options: nosniff
Referrer-Policy: strict-origin-when-cross-origin
Permissions-Policy: camera=self, microphone=self...
```

## Further Reading

- [MDN: Content Security Policy](https://developer.mozilla.org/en-US/docs/Web/HTTP/CSP)
- [MDN: HTTP Strict Transport Security](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Strict-Transport-Security)
- [MDN: X-Frame-Options](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/X-Frame-Options)
- [MDN: Referrer-Policy](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Referrer-Policy)
- [MDN: Permissions-Policy](https://developer.mozilla.org/en-US/docs/Web/HTTP/Headers/Permissions-Policy)
- [OWASP Secure Headers Project](https://owasp.org/www-project-secure-headers/)
