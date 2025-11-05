namespace Suave

open System
open System.Collections.Generic
open Suave

/// Module for efficient HTTP routing with path parameter support
module Router =

  /// Represents a segment in a route pattern
  type RouteSegment =
    | Literal of string
    | Parameter of string
    | Wildcard of string

  /// Represents a parsed route pattern
  type RoutePattern = {
    segments: RouteSegment list
    isExact: bool  // true if no parameters or wildcards
  }

  /// Route entry with pattern and handler
  type RouteEntry = {
    pattern: RoutePattern
    methods: HttpMethod list
    handler: WebPart
  }

  /// Router state containing all registered routes
  type Router = {
    exactRoutes: Dictionary<string * HttpMethod, WebPart>
    patternRoutes: RouteEntry list
  }

  /// Parse a route pattern string into segments
  let parsePattern (pattern: string) : RoutePattern =
    let segments =
      pattern.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
      |> Array.map (fun seg ->
        if seg.StartsWith(":") then
          Parameter (seg.Substring(1))
        elif seg = "*" || seg.StartsWith("*") then
          let name = if seg.Length > 1 then seg.Substring(1) else "path"
          Wildcard name
        else
          Literal seg)
      |> Array.toList
    
    let isExact = segments |> List.forall (function Literal _ -> true | _ -> false)
    { segments = segments; isExact = isExact }

  /// Match a request path against a route pattern and extract parameters
  let matchPattern (pattern: RoutePattern) (path: string) : (string * string) list option =
    let pathSegments =
      path.Split([|'/'|], StringSplitOptions.RemoveEmptyEntries)
      |> Array.toList
    
    let rec matchSegments patternSegs pathSegs acc =
      match patternSegs, pathSegs with
      | [], [] -> Some (List.rev acc)
      | [], _ -> None
      | Wildcard name :: _, remaining ->
          let wildcardPath = String.concat "/" remaining
          Some (List.rev ((name, wildcardPath) :: acc))
      | Literal lit :: pRest, p :: pathRest when lit = p ->
          matchSegments pRest pathRest acc
      | Parameter param :: pRest, p :: pathRest ->
          matchSegments pRest pathRest ((param, p) :: acc)
      | _ -> None
    
    matchSegments pattern.segments pathSegments []

  /// Create an empty router
  let empty () : Router =
    { exactRoutes = Dictionary<string * HttpMethod, WebPart>()
      patternRoutes = [] }

  /// Add a route to the router
  let addRoute (router: Router) (pattern: string) (methods: HttpMethod list) (handler: WebPart) : Router =
    let parsed = parsePattern pattern
    
    if parsed.isExact then
      // Exact match - add to dictionary for O(1) lookup
      let normalizedPath = if pattern.StartsWith("/") then pattern else "/" + pattern
      for method in methods do
        router.exactRoutes.[(normalizedPath, method)] <- handler
      router
    else
      // Pattern route - add to pattern list
      let entry = { pattern = parsed; methods = methods; handler = handler }
      { router with patternRoutes = router.patternRoutes @ [entry] }

  /// Try to match and execute a route
  let tryRoute (router: Router) (ctx: HttpContext) : Async<HttpContext option> =
    async {
      let path = ctx.request.path
      let method = ctx.request.method
      
      // First try exact match (O(1))
      if router.exactRoutes.ContainsKey(path, method) then
        let handler = router.exactRoutes.[path, method]
        return! handler ctx
      else
        // Try pattern routes in order
        let rec tryPatterns = function
          | [] -> async { return None }
          | entry :: rest ->
              async {
                if entry.methods |> List.contains method then
                  match matchPattern entry.pattern path with
                  | Some parameters ->
                      // Store parameters in userState for retrieval
                      for (key, value) in parameters do
                        ctx.userState.[sprintf "route_%s" key] <- value
                      let! result = entry.handler ctx
                      match result with
                      | Some _ as res -> return res
                      | None -> return! tryPatterns rest
                  | None ->
                      return! tryPatterns rest
                else
                  return! tryPatterns rest
              }
        
        return! tryPatterns router.patternRoutes
    }

  /// Get a route parameter from context
  let routeParam (name: string) (ctx: HttpContext) : string option =
    let key = sprintf "route_%s" name
    match ctx.userState.TryGetValue(key) with
    | true, value -> Some (value :?> string)
    | false, _ -> None

  /// Builder for creating routers with a clean syntax
  type RouterBuilder() =
    member _.Yield _ = empty()
    
    member _.Run(router: Router) : WebPart =
      fun ctx -> tryRoute router ctx
    
    [<CustomOperation("get")>]
    member _.Get(router: Router, path: string, handler: WebPart) =
      addRoute router path [HttpMethod.GET] handler
    
    [<CustomOperation("post")>]
    member _.Post(router: Router, path: string, handler: WebPart) =
      addRoute router path [HttpMethod.POST] handler
    
    [<CustomOperation("put")>]
    member _.Put(router: Router, path: string, handler: WebPart) =
      addRoute router path [HttpMethod.PUT] handler
    
    [<CustomOperation("delete")>]
    member _.Delete(router: Router, path: string, handler: WebPart) =
      addRoute router path [HttpMethod.DELETE] handler
    
    [<CustomOperation("patch")>]
    member _.Patch(router: Router, path: string, handler: WebPart) =
      addRoute router path [HttpMethod.PATCH] handler
    
    [<CustomOperation("head")>]
    member _.Head(router: Router, path: string, handler: WebPart) =
      addRoute router path [HttpMethod.HEAD] handler
    
    [<CustomOperation("options")>]
    member _.Options(router: Router, path: string, handler: WebPart) =
      addRoute router path [HttpMethod.OPTIONS] handler
    
    [<CustomOperation("route")>]
    member _.Route(router: Router, (methods: HttpMethod list, path: string, handler: WebPart)) =
      addRoute router path methods handler

  /// Computation expression for building routers
  let router = RouterBuilder()

  /// Helper to create a route for multiple HTTP methods
  let route (methods: HttpMethod list) (path: string) (handler: WebPart) =
    (methods, path, handler)

  /// Scope builder for route prefixes
  type ScopeBuilder(prefix: string) =
    let addPrefix (path: string) =
      let p = if path.StartsWith("/") then path else "/" + path
      if prefix.EndsWith("/") then prefix + p.TrimStart('/') else prefix + p
    
    member _.Yield _ = empty()
    
    member _.Run(router: Router) : Router = router
    
    [<CustomOperation("get")>]
    member _.Get(router: Router, path: string, handler: WebPart) =
      addRoute router (addPrefix path) [HttpMethod.GET] handler
    
    [<CustomOperation("post")>]
    member _.Post(router: Router, path: string, handler: WebPart) =
      addRoute router (addPrefix path) [HttpMethod.POST] handler
    
    [<CustomOperation("put")>]
    member _.Put(router: Router, path: string, handler: WebPart) =
      addRoute router (addPrefix path) [HttpMethod.PUT] handler
    
    [<CustomOperation("delete")>]
    member _.Delete(router: Router, path: string, handler: WebPart) =
      addRoute router (addPrefix path) [HttpMethod.DELETE] handler
    
    [<CustomOperation("patch")>]
    member _.Patch(router: Router, path: string, handler: WebPart) =
      addRoute router (addPrefix path) [HttpMethod.PATCH] handler

  /// Create a scope with a route prefix
  let scope (prefix: string) = ScopeBuilder(prefix)
