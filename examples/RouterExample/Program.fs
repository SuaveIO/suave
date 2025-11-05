module RouterExample.Program

open Suave
open Suave.Successful
open Suave.Router
open Suave.Operators
open Suave.Filters

// Sample data
type User = { id: int; name: string; email: string }

let users = 
    [ { id = 1; name = "Alice"; email = "alice@example.com" }
      { id = 2; name = "Bob"; email = "bob@example.com" }
      { id = 3; name = "Charlie"; email = "charlie@example.com" } ]

// Handler functions
let listUsers = 
    let userList = 
        users 
        |> List.map (fun u -> sprintf "- %d: %s (%s)" u.id u.name u.email)
        |> String.concat "\n"
    OK (sprintf "Users:\n%s\n\nTry: /users/:id or /users/:id/profile" userList)

let getUser (ctx: HttpContext) =
    match routeParam "id" ctx with
    | Some idStr ->
        match System.Int32.TryParse(idStr) with
        | true, id ->
            match users |> List.tryFind (fun u -> u.id = id) with
            | Some user ->
                OK (sprintf "User #%d\nName: %s\nEmail: %s" user.id user.name user.email) ctx
            | None ->
                RequestErrors.NOT_FOUND (sprintf "User %d not found" id) ctx
        | false, _ ->
            RequestErrors.BAD_REQUEST "Invalid user ID" ctx
    | None ->
        RequestErrors.BAD_REQUEST "Missing user ID" ctx

let getUserProfile (ctx: HttpContext) =
    match routeParam "id" ctx with
    | Some idStr ->
        OK (sprintf "Profile page for user %s\n\nThis demonstrates nested path parameters." idStr) ctx
    | None ->
        RequestErrors.BAD_REQUEST "Missing user ID" ctx

let createUser = OK "Create user (POST)"
let updateUser (ctx: HttpContext) =
    match routeParam "id" ctx with
    | Some id -> OK (sprintf "Update user %s (PUT)" id) ctx
    | None -> RequestErrors.BAD_REQUEST "Missing user ID" ctx

let deleteUser (ctx: HttpContext) =
    match routeParam "id" ctx with
    | Some id -> OK (sprintf "Delete user %s (DELETE)" id) ctx
    | None -> RequestErrors.BAD_REQUEST "Missing user ID" ctx

let staticFiles (ctx: HttpContext) =
    match routeParam "filepath" ctx with
    | Some path -> OK (sprintf "Serving static file: %s" path) ctx
    | None -> RequestErrors.BAD_REQUEST "No file path" ctx

let about = OK "About page"
let contact = OK "Contact page"

let home = Writers.setMimeType "text/html; charset=utf-8" >=> OK """
🚀 <h1>Suave Router Example</h1>
<br/>
This demonstrates the new efficient routing system with:<br/>
- O(1) exact route matching<br/>
- Path parameter support (:id)<br/>
- Wildcard matching (*)<br/>
- Route prefixes/scoping<br/>
<br/>
Try these routes:<br/>
<br/>

Exact routes (O(1) lookup):<br/>
  GET  /                    (this page)<br/>
  GET  /about               (about page)<br/>
  GET  /contact             (contact page)<br/>
<br/>
Pattern routes with parameters:<br/>
  GET  /users               (list all users)<br/>
  GET  /users/:id           (get specific user, try: /users/1)<br/>
  GET  /users/:id/profile   (user profile page)<br/>
  POST /users               (create user)<br/>
  PUT  /users/:id           (update user)<br/>
  DELETE /users/:id         (delete user)<br/>
<br/>
API routes (with prefix):<br/>
  GET  /api/v1/users        (API user list)<br/>
  GET  /api/v1/users/:id    (API get user)<br/>
<br/>
Wildcard routes:<br/>
  GET  /static/*filepath    (serve static files, try: /static/css/style.css)<br/>
<br/>
Performance: <br/>
- Exact routes use Dictionary for O(1) lookup<br/>
- Pattern routes checked sequentially<br/>
- Backwards compatible with existing choose-based routing!
"""

[<EntryPoint>]
let main argv =
    printfn "==========================================="
    printfn "  Suave Router Performance Demo"
    printfn "==========================================="
    printfn ""
    printfn "Server: http://localhost:8080"
    printfn ""
    printfn "Features demonstrated:"
    printfn "  ✓ O(1) exact route matching"
    printfn "  ✓ Path parameters (:id)"
    printfn "  ✓ Wildcard routes (*path)"
    printfn "  ✓ Route prefixes/scoping"
    printfn "  ✓ Clean computation expression syntax"
    printfn ""
    printfn "Open http://localhost:8080 for examples"
    printfn "==========================================="
    printfn ""
    
    // Build the application using the new router
    let app =
        router {
            // Exact routes - O(1) dictionary lookup
            get  "/"              home
            get  "/about"         about
            get  "/contact"       contact
            
            // Routes with path parameters
            get    "/users"          listUsers
            get    "/users/:id"      getUser
            get    "/users/:id/profile" getUserProfile
            post   "/users"          createUser
            put    "/users/:id"      updateUser
            delete "/users/:id"      deleteUser
            
            // Wildcard route
            get  "/static/*filepath"  staticFiles
            
            // API routes with scope (prefix)
            // Note: scope currently needs to be expanded manually
            // This is a limitation we could improve
            get "/api/v1/users"      (OK "API: List users")
            get "/api/v1/users/:id"  (fun ctx ->
                match routeParam "id" ctx with
                | Some id -> OK (sprintf "API: Get user %s" id) ctx
                | None -> RequestErrors.BAD_REQUEST "Missing ID" ctx)
        }
    
    let config = { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 8080 ] }
    startWebServer config app
    0
