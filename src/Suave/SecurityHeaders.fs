namespace Suave

open System

/// Security Headers module for comprehensive HTTP security header management
module SecurityHeaders =

  /// Content Security Policy Level 3 directives
  [<RequireQualifiedAccess>]
  type CSPDirective =
    | DefaultSrc
    | ScriptSrc
    | ScriptSrcElem
    | ScriptSrcAttr
    | StyleSrc
    | StyleSrcElem
    | StyleSrcAttr
    | ImgSrc
    | FontSrc
    | ConnectSrc
    | MediaSrc
    | ObjectSrc
    | FrameSrc
    | ChildSrc
    | FrameAncestors
    | FormAction
    | BaseUri
    | ManifestSrc
    | WorkerSrc
    | NavigateTo
    | ReportTo
    | RequireTrustedTypesFor
    | TrustedTypes
    | UpgradeInsecureRequests
    | BlockAllMixedContent
    
    member this.Name =
      match this with
      | DefaultSrc -> "default-src"
      | ScriptSrc -> "script-src"
      | ScriptSrcElem -> "script-src-elem"
      | ScriptSrcAttr -> "script-src-attr"
      | StyleSrc -> "style-src"
      | StyleSrcElem -> "style-src-elem"
      | StyleSrcAttr -> "style-src-attr"
      | ImgSrc -> "img-src"
      | FontSrc -> "font-src"
      | ConnectSrc -> "connect-src"
      | MediaSrc -> "media-src"
      | ObjectSrc -> "object-src"
      | FrameSrc -> "frame-src"
      | ChildSrc -> "child-src"
      | FrameAncestors -> "frame-ancestors"
      | FormAction -> "form-action"
      | BaseUri -> "base-uri"
      | ManifestSrc -> "manifest-src"
      | WorkerSrc -> "worker-src"
      | NavigateTo -> "navigate-to"
      | ReportTo -> "report-to"
      | RequireTrustedTypesFor -> "require-trusted-types-for"
      | TrustedTypes -> "trusted-types"
      | UpgradeInsecureRequests -> "upgrade-insecure-requests"
      | BlockAllMixedContent -> "block-all-mixed-content"

  /// CSP source values
  [<RequireQualifiedAccess>]
  type CSPSource =
    | Self
    | None
    | UnsafeInline
    | UnsafeEval
    | UnsafeHashes
    | StrictDynamic
    | ReportSample
    | Nonce of string
    | Hash of algorithm: string * value: string
    | Scheme of string
    | Host of string
    | Custom of string
    
    member this.Value =
      match this with
      | Self -> "'self'"
      | None -> "'none'"
      | UnsafeInline -> "'unsafe-inline'"
      | UnsafeEval -> "'unsafe-eval'"
      | UnsafeHashes -> "'unsafe-hashes'"
      | StrictDynamic -> "'strict-dynamic'"
      | ReportSample -> "'report-sample'"
      | Nonce n -> sprintf "'nonce-%s'" n
      | Hash (alg, value) -> sprintf "'%s-%s'" alg value
      | Scheme s -> sprintf "%s:" s
      | Host h -> h
      | Custom c -> c

  /// CSP directive with sources
  type CSPRule =
    { directive: CSPDirective
      sources: CSPSource list }

  /// Content Security Policy configuration
  type CSPConfig =
    { rules: CSPRule list
      reportOnly: bool }
    
    static member Empty = { rules = []; reportOnly = false }

  /// X-Frame-Options values
  type FrameOptions =
    | Deny
    | SameOrigin
    | AllowFrom of string
    
    member this.Value =
      match this with
      | Deny -> "DENY"
      | SameOrigin -> "SAMEORIGIN"
      | AllowFrom uri -> sprintf "ALLOW-FROM %s" uri

  /// Referrer-Policy values
  type ReferrerPolicy =
    | NoReferrer
    | NoReferrerWhenDowngrade
    | Origin
    | OriginWhenCrossOrigin
    | SameOrigin
    | StrictOrigin
    | StrictOriginWhenCrossOrigin
    | UnsafeUrl
    
    member this.Value =
      match this with
      | NoReferrer -> "no-referrer"
      | NoReferrerWhenDowngrade -> "no-referrer-when-downgrade"
      | Origin -> "origin"
      | OriginWhenCrossOrigin -> "origin-when-cross-origin"
      | SameOrigin -> "same-origin"
      | StrictOrigin -> "strict-origin"
      | StrictOriginWhenCrossOrigin -> "strict-origin-when-cross-origin"
      | UnsafeUrl -> "unsafe-url"

  /// Permissions-Policy directive
  type PermissionsDirective =
    | Accelerometer
    | AmbientLightSensor
    | Autoplay
    | Battery
    | Camera
    | DisplayCapture
    | DocumentDomain
    | EncryptedMedia
    | Fullscreen
    | Geolocation
    | Gyroscope
    | Magnetometer
    | Microphone
    | Midi
    | Payment
    | PictureInPicture
    | PublicKeyCredentials
    | ScreenWakeLock
    | SyncXhr
    | Usb
    | WebShare
    | XrSpatialTracking
    
    member this.Name =
      match this with
      | Accelerometer -> "accelerometer"
      | AmbientLightSensor -> "ambient-light-sensor"
      | Autoplay -> "autoplay"
      | Battery -> "battery"
      | Camera -> "camera"
      | DisplayCapture -> "display-capture"
      | DocumentDomain -> "document-domain"
      | EncryptedMedia -> "encrypted-media"
      | Fullscreen -> "fullscreen"
      | Geolocation -> "geolocation"
      | Gyroscope -> "gyroscope"
      | Magnetometer -> "magnetometer"
      | Microphone -> "microphone"
      | Midi -> "midi"
      | Payment -> "payment"
      | PictureInPicture -> "picture-in-picture"
      | PublicKeyCredentials -> "publickey-credentials"
      | ScreenWakeLock -> "screen-wake-lock"
      | SyncXhr -> "sync-xhr"
      | Usb -> "usb"
      | WebShare -> "web-share"
      | XrSpatialTracking -> "xr-spatial-tracking"

  /// Permissions-Policy allowlist
  type PermissionsAllowlist =
    | AllOrigins
    | SelfOnly
    | NoOrigins
    | Origins of string list
    
    member this.Value =
      match this with
      | AllOrigins -> "*"
      | SelfOnly -> "self"
      | NoOrigins -> "()"
      | Origins origins -> sprintf "(%s)" (String.concat " " origins)

  /// Permissions-Policy rule
  type PermissionsRule =
    { directive: PermissionsDirective
      allowlist: PermissionsAllowlist }

  /// HSTS configuration
  type HSTSConfig =
    { maxAge: int
      includeSubDomains: bool
      preload: bool }

  /// X-Permitted-Cross-Domain-Policies
  type CrossDomainPolicy =
    | NonePolicy
    | MasterOnly
    | ByContentType
    | AllPolicies
    
    member this.Value =
      match this with
      | NonePolicy -> "none"
      | MasterOnly -> "master-only"
      | ByContentType -> "by-content-type"
      | AllPolicies -> "all"

  /// Cache-Control security settings
  type CacheControl =
    | NoStore
    | NoCache
    | MustRevalidate
    | NoStoreNoCache
    | Private
    
    member this.Value =
      match this with
      | NoStore -> "no-store"
      | NoCache -> "no-cache"
      | MustRevalidate -> "must-revalidate"
      | NoStoreNoCache -> "no-store, no-cache"
      | Private -> "private"

  /// Comprehensive security headers configuration
  type SecurityHeadersConfig =
    { contentSecurityPolicy: CSPConfig option
      strictTransportSecurity: HSTSConfig option
      xFrameOptions: FrameOptions option
      xContentTypeOptions: bool
      referrerPolicy: ReferrerPolicy option
      permissionsPolicy: PermissionsRule list
      xXssProtection: bool option  // None = don't set, Some true = "1; mode=block", Some false = "0"
      xPermittedCrossDomainPolicies: CrossDomainPolicy option
      cacheControl: CacheControl option }

  /// Build CSP header value from configuration
  let private buildCSPHeader (config: CSPConfig) : string =
    config.rules
    |> List.map (fun rule ->
      match rule.sources with
      | [] -> 
          // Directives without sources (like upgrade-insecure-requests)
          rule.directive.Name
      | sources ->
          sprintf "%s %s" 
            rule.directive.Name 
            (sources |> List.map (fun s -> s.Value) |> String.concat " "))
    |> String.concat "; "

  /// Build HSTS header value
  let private buildHSTSHeader (config: HSTSConfig) : string =
    let parts = [
      sprintf "max-age=%d" config.maxAge
      if config.includeSubDomains then "includeSubDomains"
      if config.preload then "preload"
    ]
    String.concat "; " parts

  /// Build Permissions-Policy header value
  let private buildPermissionsPolicyHeader (rules: PermissionsRule list) : string =
    rules
    |> List.map (fun rule -> 
      sprintf "%s=%s" rule.directive.Name rule.allowlist.Value)
    |> String.concat ", "

  /// Generate a cryptographically random nonce for CSP
  let generateNonce() : string =
    let bytes = Array.zeroCreate<byte> 16
    use rng = System.Security.Cryptography.RandomNumberGenerator.Create()
    rng.GetBytes(bytes)
    Convert.ToBase64String(bytes)

  /// Apply security headers WebPart
  let securityHeaders (config: SecurityHeadersConfig) : WebPart =
    fun ctx ->
      let mutable ctx' = ctx
      
      // Content-Security-Policy
      match config.contentSecurityPolicy with
      | Some csp ->
          let headerName = if csp.reportOnly then "Content-Security-Policy-Report-Only" else "Content-Security-Policy"
          let headerValue = buildCSPHeader csp
          ctx' <- { ctx' with response = { ctx'.response with headers = (headerName, headerValue) :: ctx'.response.headers }}
      | None -> ()
      
      // Strict-Transport-Security
      match config.strictTransportSecurity with
      | Some hsts ->
          let headerValue = buildHSTSHeader hsts
          ctx' <- { ctx' with response = { ctx'.response with headers = ("Strict-Transport-Security", headerValue) :: ctx'.response.headers }}
      | None -> ()
      
      // X-Frame-Options
      match config.xFrameOptions with
      | Some frameOpts ->
          ctx' <- { ctx' with response = { ctx'.response with headers = ("X-Frame-Options", frameOpts.Value) :: ctx'.response.headers }}
      | None -> ()
      
      // X-Content-Type-Options
      if config.xContentTypeOptions then
        ctx' <- { ctx' with response = { ctx'.response with headers = ("X-Content-Type-Options", "nosniff") :: ctx'.response.headers }}
      
      // Referrer-Policy
      match config.referrerPolicy with
      | Some policy ->
          ctx' <- { ctx' with response = { ctx'.response with headers = ("Referrer-Policy", policy.Value) :: ctx'.response.headers }}
      | None -> ()
      
      // Permissions-Policy
      if not (List.isEmpty config.permissionsPolicy) then
        let headerValue = buildPermissionsPolicyHeader config.permissionsPolicy
        ctx' <- { ctx' with response = { ctx'.response with headers = ("Permissions-Policy", headerValue) :: ctx'.response.headers }}
      
      // X-XSS-Protection
      match config.xXssProtection with
      | Some true -> 
          ctx' <- { ctx' with response = { ctx'.response with headers = ("X-XSS-Protection", "1; mode=block") :: ctx'.response.headers }}
      | Some false ->
          ctx' <- { ctx' with response = { ctx'.response with headers = ("X-XSS-Protection", "0") :: ctx'.response.headers }}
      | None -> ()
      
      // X-Permitted-Cross-Domain-Policies
      match config.xPermittedCrossDomainPolicies with
      | Some policy ->
          ctx' <- { ctx' with response = { ctx'.response with headers = ("X-Permitted-Cross-Domain-Policies", policy.Value) :: ctx'.response.headers }}
      | None -> ()
      
      // Cache-Control
      match config.cacheControl with
      | Some cache ->
          ctx' <- { ctx' with response = { ctx'.response with headers = ("Cache-Control", cache.Value) :: ctx'.response.headers }}
      | None -> ()
      
      succeed ctx'

  // === Preset Configurations ===

  /// Strict security configuration - maximum protection, may break some features
  let strictConfig : SecurityHeadersConfig =
    { contentSecurityPolicy = Some {
        rules = [
          { directive = CSPDirective.DefaultSrc; sources = [CSPSource.Self] }
          { directive = CSPDirective.ScriptSrc; sources = [CSPSource.Self; CSPSource.StrictDynamic] }
          { directive = CSPDirective.StyleSrc; sources = [CSPSource.Self] }
          { directive = CSPDirective.ImgSrc; sources = [CSPSource.Self; CSPSource.Scheme "data"] }
          { directive = CSPDirective.FontSrc; sources = [CSPSource.Self] }
          { directive = CSPDirective.ConnectSrc; sources = [CSPSource.Self] }
          { directive = CSPDirective.FrameAncestors; sources = [CSPSource.None] }
          { directive = CSPDirective.BaseUri; sources = [CSPSource.Self] }
          { directive = CSPDirective.FormAction; sources = [CSPSource.Self] }
          { directive = CSPDirective.UpgradeInsecureRequests; sources = [] }
          { directive = CSPDirective.BlockAllMixedContent; sources = [] }
        ]
        reportOnly = false
      }
      strictTransportSecurity = Some {
        maxAge = 31536000  // 1 year
        includeSubDomains = true
        preload = true
      }
      xFrameOptions = Some FrameOptions.Deny
      xContentTypeOptions = true
      referrerPolicy = Some ReferrerPolicy.NoReferrer
      permissionsPolicy = [
        { directive = PermissionsDirective.Camera; allowlist = PermissionsAllowlist.NoOrigins }
        { directive = PermissionsDirective.Microphone; allowlist = PermissionsAllowlist.NoOrigins }
        { directive = PermissionsDirective.Geolocation; allowlist = PermissionsAllowlist.NoOrigins }
        { directive = PermissionsDirective.Payment; allowlist = PermissionsAllowlist.NoOrigins }
      ]
      xXssProtection = Some true
      xPermittedCrossDomainPolicies = Some CrossDomainPolicy.NonePolicy
      cacheControl = Some CacheControl.NoStoreNoCache }

  /// Balanced security configuration - reasonable defaults for most applications
  let balancedConfig : SecurityHeadersConfig =
    { contentSecurityPolicy = Some {
        rules = [
          { directive = CSPDirective.DefaultSrc; sources = [CSPSource.Self] }
          { directive = CSPDirective.ScriptSrc; sources = [CSPSource.Self] }
          { directive = CSPDirective.StyleSrc; sources = [CSPSource.Self; CSPSource.UnsafeInline] }
          { directive = CSPDirective.ImgSrc; sources = [CSPSource.Self; CSPSource.Scheme "data"; CSPSource.Scheme "https"] }
          { directive = CSPDirective.FontSrc; sources = [CSPSource.Self; CSPSource.Scheme "data"] }
          { directive = CSPDirective.ConnectSrc; sources = [CSPSource.Self] }
          { directive = CSPDirective.FrameAncestors; sources = [CSPSource.Self] }
          { directive = CSPDirective.BaseUri; sources = [CSPSource.Self] }
          { directive = CSPDirective.FormAction; sources = [CSPSource.Self] }
          { directive = CSPDirective.UpgradeInsecureRequests; sources = [] }
        ]
        reportOnly = false
      }
      strictTransportSecurity = Some {
        maxAge = 31536000  // 1 year
        includeSubDomains = true
        preload = false
      }
      xFrameOptions = Some FrameOptions.SameOrigin
      xContentTypeOptions = true
      referrerPolicy = Some ReferrerPolicy.StrictOriginWhenCrossOrigin
      permissionsPolicy = [
        { directive = PermissionsDirective.Camera; allowlist = PermissionsAllowlist.SelfOnly }
        { directive = PermissionsDirective.Microphone; allowlist = PermissionsAllowlist.SelfOnly }
        { directive = PermissionsDirective.Geolocation; allowlist = PermissionsAllowlist.SelfOnly }
      ]
      xXssProtection = Some true
      xPermittedCrossDomainPolicies = Some CrossDomainPolicy.NonePolicy
      cacheControl = None }

  /// Relaxed security configuration - minimal restrictions
  let relaxedConfig : SecurityHeadersConfig =
    { contentSecurityPolicy = Some {
        rules = [
          { directive = CSPDirective.DefaultSrc; sources = [CSPSource.Self; CSPSource.UnsafeInline; CSPSource.UnsafeEval] }
          { directive = CSPDirective.ImgSrc; sources = [CSPSource.Custom "*"] }
          { directive = CSPDirective.FrameAncestors; sources = [CSPSource.Self] }
        ]
        reportOnly = false
      }
      strictTransportSecurity = Some {
        maxAge = 86400  // 1 day
        includeSubDomains = false
        preload = false
      }
      xFrameOptions = Some FrameOptions.SameOrigin
      xContentTypeOptions = true
      referrerPolicy = Some ReferrerPolicy.NoReferrerWhenDowngrade
      permissionsPolicy = []
      xXssProtection = Some true
      xPermittedCrossDomainPolicies = None
      cacheControl = None }

  /// Empty configuration - no security headers
  let emptyConfig : SecurityHeadersConfig =
    { contentSecurityPolicy = None
      strictTransportSecurity = None
      xFrameOptions = None
      xContentTypeOptions = false
      referrerPolicy = None
      permissionsPolicy = []
      xXssProtection = None
      xPermittedCrossDomainPolicies = None
      cacheControl = None }

  // === Helper functions for building configurations ===

  /// Add a CSP rule to configuration
  let withCSPRule (directive: CSPDirective) (sources: CSPSource list) (config: SecurityHeadersConfig) =
    let csp = 
      match config.contentSecurityPolicy with
      | Some csp -> csp
      | None -> CSPConfig.Empty
    
    let newRule = { directive = directive; sources = sources }
    let updatedRules = newRule :: csp.rules
    
    { config with contentSecurityPolicy = Some { csp with rules = updatedRules } }

  /// Set CSP to report-only mode
  let withCSPReportOnly (reportOnly: bool) (config: SecurityHeadersConfig) =
    match config.contentSecurityPolicy with
    | Some csp ->
        { config with contentSecurityPolicy = Some { csp with reportOnly = reportOnly } }
    | None -> config

  /// Set HSTS configuration
  let withHSTS (maxAge: int) (includeSubDomains: bool) (preload: bool) (config: SecurityHeadersConfig) =
    { config with strictTransportSecurity = Some { maxAge = maxAge; includeSubDomains = includeSubDomains; preload = preload } }

  /// Set X-Frame-Options
  let withFrameOptions (options: FrameOptions) (config: SecurityHeadersConfig) =
    { config with xFrameOptions = Some options }

  /// Set Referrer-Policy
  let withReferrerPolicy (policy: ReferrerPolicy) (config: SecurityHeadersConfig) =
    { config with referrerPolicy = Some policy }

  /// Add a Permissions-Policy rule
  let withPermissionsRule (directive: PermissionsDirective) (allowlist: PermissionsAllowlist) (config: SecurityHeadersConfig) =
    let newRule = { directive = directive; allowlist = allowlist }
    { config with permissionsPolicy = newRule :: config.permissionsPolicy }

  /// Set Cache-Control for security
  let withSecureCache (cache: CacheControl) (config: SecurityHeadersConfig) =
    { config with cacheControl = Some cache }
