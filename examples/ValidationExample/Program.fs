module ValidationExample.Program

open Suave
open Suave.Filters
open Suave.Successful
open Suave.RequestErrors
open Suave.Validation
open Suave.ValidationWebParts
open Suave.Operators

let htmlForm = """
    <!DOCTYPE html>
    <html>
    <head>
        <meta charset="UTF-8">
        <title>Suave Validation Framework Example</title>
        <style>
            body { 
                font-family: 'Segoe UI', Arial, sans-serif; 
                max-width: 700px; 
                margin: 50px auto; 
                padding: 20px;
                background: #f5f5f5;
            }
            .container {
                background: white;
                padding: 30px;
                border-radius: 8px;
                box-shadow: 0 2px 4px rgba(0,0,0,0.1);
            }
            h1 { color: #333; border-bottom: 3px solid #4CAF50; padding-bottom: 10px; }
            h2 { color: #666; margin-top: 30px; }
            form { margin: 15px 0; }
            input, button { 
                margin: 8px 0; 
                padding: 12px; 
                width: 100%; 
                box-sizing: border-box;
                border: 1px solid #ddd;
                border-radius: 4px;
            }
            button { 
                background: #4CAF50; 
                color: white; 
                cursor: pointer; 
                font-weight: bold;
                border: none;
            }
            button:hover { background: #45a049; }
            .success { color: #4CAF50; font-weight: bold; }
            .error { color: #f44336; font-weight: bold; }
            a { color: #4CAF50; text-decoration: none; }
            a:hover { text-decoration: underline; }
        </style>
    </head>
    <body>
        <div class="container">
            <h1>🛡️ Suave Input Validation Framework</h1>
            <p>This example demonstrates the new validation framework with various validation scenarios.</p>
            
            <h2>1. Email Validation</h2>
            <p><small>Tests: not empty, valid email format</small></p>
            <form action="/validate-email" method="post">
                <input type="text" name="email" placeholder="Enter email address" required>
                <button type="submit">Validate Email</button>
            </form>
            
            <h2>2. Password Strength</h2>
            <p><small>Tests: not empty, minimum length (8 chars)</small></p>
            <form action="/validate-password" method="post">
                <input type="password" name="password" placeholder="Enter password (min 8 characters)" required>
                <button type="submit">Validate Password</button>
            </form>
            
            <h2>3. Age Range Validation</h2>
            <p><small>Tests: integer parsing, range validation (18-100)</small></p>
            <form action="/validate-age" method="post">
                <input type="number" name="age" placeholder="Enter age (18-100)" required>
                <button type="submit">Validate Age</button>
            </form>
            
            <h2>4. Username Validation</h2>
            <p><small>Tests: alphanumeric only, length between 3-20 chars</small></p>
            <form action="/validate-username" method="post">
                <input type="text" name="username" placeholder="Enter username (3-20 alphanumeric)" required>
                <button type="submit">Validate Username</button>
            </form>
            
            <h2>5. Search Query</h2>
            <p><small>Tests: query parameter validation, not empty</small></p>
            <form action="/search" method="get">
                <input type="text" name="q" placeholder="Enter search query" required>
                <button type="submit">Search</button>
            </form>
            
            <h2>6. URL Validation</h2>
            <p><small>Tests: valid URL format</small></p>
            <form action="/validate-url" method="post">
                <input type="text" name="url" placeholder="Enter URL (e.g., https://example.com)" required>
                <button type="submit">Validate URL</button>
            </form>
            
            <h2>7. Date Validation</h2>
            <p><small>Tests: date parsing, future date check</small></p>
            <form action="/validate-date" method="post">
                <input type="date" name="date" required>
                <button type="submit">Validate Date</button>
            </form>
            
            <h2>8. Decimal/Price Validation</h2>
            <p><small>Tests: decimal parsing, positive value</small></p>
            <form action="/validate-price" method="post">
                <input type="text" name="price" placeholder="Enter price (e.g., 19.99)" required>
                <button type="submit">Validate Price</button>
            </form>
            
            <h2>9. Boolean Validation</h2>
            <p><small>Tests: boolean parsing</small></p>
            <form action="/validate-bool" method="post">
                <select name="agreement">
                    <option value="">Select</option>
                    <option value="true">I agree</option>
                    <option value="false">I disagree</option>
                </select>
                <button type="submit">Validate Agreement</button>
            </form>
            
            <h2>10. GUID Validation</h2>
            <p><small>Tests: GUID parsing</small></p>
            <form action="/validate-guid" method="post">
                <input type="text" name="guid" placeholder="Enter GUID" required>
                <button type="submit">Validate GUID</button>
            </form>
            
            <h2>11. Tag Selection (oneOf)</h2>
            <p><small>Tests: value must be one of allowed options</small></p>
            <form action="/validate-tag" method="post">
                <select name="tag">
                    <option value="">Select a tag</option>
                    <option value="bug">Bug</option>
                    <option value="feature">Feature</option>
                    <option value="docs">Documentation</option>
                    <option value="invalid">Invalid Option</option>
                </select>
                <button type="submit">Validate Tag</button>
            </form>
            
            <h2>12. Code Pattern (Regex)</h2>
            <p><small>Tests: matches specific pattern (e.g., ABC-1234)</small></p>
            <form action="/validate-code" method="post">
                <input type="text" name="code" placeholder="Enter code (ABC-1234 format)" required>
                <button type="submit">Validate Code</button>
            </form>
        </div>
    </body>
    </html>
    """

// Email validation endpoint - showcases validateFormField WebPart
let validateEmailHandler : WebPart = 
    validateFormField "email" (String.email "email")
    >=> fun ctx ->
        let email = getValidated<string> "email" ctx |> Option.defaultValue ""
        let response = sprintf $"""
            <div class="container">
                <p class="success">✓ Valid email address!</p>
                <p><strong>Email:</strong> {email}</p>
                <p><em>Using: validateFormField WebPart</em></p>
                <p><a href="/">← Back to examples</a></p>
            </div>
            """
        OK response ctx

// Password validation endpoint
let validatePasswordHandler : WebPart =
    fun ctx -> async {
        match ctx.request.formData "password" with
        | Choice1Of2 password ->
            match String.minLength 8 "password" password with
            | Valid _ ->
                let response = sprintf $"""
                    <div class="container">
                        <p class="success">✓ Password meets requirements!</p>
                        <p><strong>Length:</strong> {password.Length} characters</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! OK response ctx
            | Invalid errors ->
                let errorMsg = errors |> List.head
                let response = sprintf $"""
                    <div class="container">
                        <p class="error">✗ Password validation failed</p>
                        <p><strong>Error:</strong> {errorMsg.message}</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! BAD_REQUEST response ctx
        | Choice2Of2 _ ->
            return! BAD_REQUEST "<div class='container'><p class='error'>✗ Missing password field</p></div>" ctx
    }

// Age validation endpoint (with parsing and range check)
let validateAgeHandler : WebPart =
    fun ctx -> async {
        match ctx.request.formData "age" with
        | Choice1Of2 ageStr ->
            // First parse to int
            match Parse.parseInt "age" ageStr with
            | Valid age ->
                // Then validate range
                match Numeric.between 18 100 "age" age with
                | Valid validAge ->
                    let response = sprintf $"""
                        <div class="container">
                            <p class="success">✓ Valid age!</p>
                            <p><strong>Age:</strong> {validAge} years old</p>
                            <p><a href="/">← Back to examples</a></p>
                        </div>
                        """
                    return! OK response ctx
                | Invalid errors ->
                    let errorMsg = errors |> List.head
                    let response = sprintf $"""
                        <div class="container">
                            <p class="error">✗ Age validation failed</p>
                            <p><strong>Error:</strong> {errorMsg.message}</p>
                            <p><a href="/">← Back to examples</a></p>
                        </div>
                        """
                    return! BAD_REQUEST response ctx
            | Invalid errors ->
                let errorMsg = errors |> List.head
                let response = sprintf $"""
                    <div class="container">
                        <p class="error">✗ Age parsing failed</p>
                        <p><strong>Error:</strong> {errorMsg.message}</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! BAD_REQUEST response ctx
        | Choice2Of2 _ ->
            return! BAD_REQUEST "<div class='container'><p class='error'>✗ Missing age field</p></div>" ctx
    }

// Username validation (alphanumeric and length)
let validateUsernameHandler : WebPart =
    fun ctx -> async {
        match ctx.request.formData "username" with
        | Choice1Of2 username ->
            // Check alphanumeric
            match String.alphanumeric "username" username with
            | Valid _ ->
                // Check length
                match String.lengthBetween 3 20 "username" username with
                | Valid validUsername ->
                    let response = sprintf $"""
                        <div class="container">
                            <p class="success">✓ Valid username!</p>
                            <p><strong>Username:</strong> {validUsername}</p>
                            <p><a href="/">← Back to examples</a></p>
                        </div>
                        """
                    return! OK response ctx
                | Invalid errors ->
                    let errorMsg = errors |> List.head
                    let response = sprintf $"""
                        <div class="container">
                            <p class="error">✗ Username length validation failed</p>
                            <p><strong>Error:</strong> {errorMsg.message}</p>
                            <p><a href="/">← Back to examples</a></p>
                        </div>
                        """
                    return! BAD_REQUEST response ctx
            | Invalid errors ->
                let errorMsg = errors |> List.head
                let response = sprintf $"""
                    <div class="container">
                        <p class="error">✗ Username format validation failed</p>
                        <p><strong>Error:</strong> {errorMsg.message}</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! BAD_REQUEST response ctx
        | Choice2Of2 _ ->
            return! BAD_REQUEST "<div class='container'><p class='error'>✗ Missing username field</p></div>" ctx
    }

// Search query validation - showcases validateQueryParam WebPart
let searchHandler : WebPart =
    validateQueryParam "q" (String.notEmpty "q")
    >=> fun ctx ->
        let query = getValidated<string> "q" ctx |> Option.defaultValue ""
        let response = sprintf $"""
            <div class="container">
                <p class="success">Search results for: <strong>{query}</strong></p>
                <p><em>(This is a demo - no actual search performed)</em></p>
                <p><em>Using: validateQueryParam WebPart</em></p>
                <p><a href="/">← Back to examples</a></p>
            </div>
            """
        OK response ctx

// URL validation - showcases validateFormField WebPart
let validateUrlHandler : WebPart =
    validateFormField "url" (String.url "url")
    >=> fun ctx ->
        let url = getValidated<string> "url" ctx |> Option.defaultValue ""
        let response = sprintf $"""
            <div class="container">
                <p class="success">✓ Valid URL!</p>
                <p><strong>URL:</strong> {url}</p>
                <p><em>Using: validateFormField WebPart</em></p>
                <p><a href="/">← Back to examples</a></p>
            </div>
            """
        OK response ctx

// Date validation endpoint (must be in the future)
let validateDateHandler : WebPart =
    fun ctx -> async {
        match ctx.request.formData "date" with
        | Choice1Of2 dateStr ->
            match Parse.parseDateTime "date" dateStr with
            | Valid date ->
                match DateTime.future "date" date with
                | Valid validDate ->
                    let response = sprintf $"""
                        <div class="container">
                            <p class="success">✓ Valid future date!</p>
                            <p><strong>Date:</strong> {validDate.ToShortDateString()}</p>
                            <p><a href="/">← Back to examples</a></p>
                        </div>
                        """
                    return! OK response ctx
                | Invalid errors ->
                    let errorMsg = errors |> List.head
                    let response = sprintf $"""
                        <div class="container">
                            <p class="error">✗ Date must be in the future</p>
                            <p><strong>Error:</strong> {errorMsg.message}</p>
                            <p><a href="/">← Back to examples</a></p>
                        </div>
                        """
                    return! BAD_REQUEST response ctx
            | Invalid errors ->
                let errorMsg = errors |> List.head
                let response = sprintf $"""
                    <div class="container">
                        <p class="error">✗ Date parsing failed</p>
                        <p><strong>Error:</strong> {errorMsg.message}</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! BAD_REQUEST response ctx
        | Choice2Of2 _ ->
            return! BAD_REQUEST "<div class='container'><p class='error'>✗ Missing date field</p></div>" ctx
    }

// Price validation endpoint (must be positive decimal)
let validatePriceHandler : WebPart =
    fun ctx -> async {
        match ctx.request.formData "price" with
        | Choice1Of2 priceStr ->
            match Parse.parseDecimal "price" priceStr with
            | Valid price ->
                match Numeric.positiveDecimal "price" price with
                | Valid validPrice ->
                    let response = sprintf $"""
                        <div class="container">
                            <p class="success">✓ Valid price!</p>
                            <p><strong>Price:</strong> ${validPrice:F2}</p>
                            <p><a href="/">← Back to examples</a></p>
                        </div>
                        """
                    return! OK response ctx
                | Invalid errors ->
                    let errorMsg = errors |> List.head
                    let response = sprintf $"""
                        <div class="container">
                            <p class="error">✗ Price validation failed</p>
                            <p><strong>Error:</strong> {errorMsg.message}</p>
                            <p><a href="/">← Back to examples</a></p>
                        </div>
                        """
                    return! BAD_REQUEST response ctx
            | Invalid errors ->
                let errorMsg = errors |> List.head
                let response = sprintf $"""
                    <div class="container">
                        <p class="error">✗ Price parsing failed</p>
                        <p><strong>Error:</strong> {errorMsg.message}</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! BAD_REQUEST response ctx
        | Choice2Of2 _ ->
            return! BAD_REQUEST "<div class='container'><p class='error'>✗ Missing price field</p></div>" ctx
    }

// Boolean validation endpoint
let validateBoolHandler : WebPart =
    fun ctx -> async {
        match ctx.request.formData "agreement" with
        | Choice1Of2 boolStr ->
            match Parse.parseBool "agreement" boolStr with
            | Valid agreed ->
                let response = sprintf $"""
                    <div class="container">
                        <p class="success">✓ Valid boolean value!</p>
                        <p><strong>Agreement:</strong> {if agreed then "Agreed" else "Disagreed"}</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! OK response ctx
            | Invalid errors ->
                let errorMsg = errors |> List.head
                let response = sprintf $"""
                    <div class="container">
                        <p class="error">✗ Boolean parsing failed</p>
                        <p><strong>Error:</strong> {errorMsg.message}</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! BAD_REQUEST response ctx
        | Choice2Of2 _ ->
            return! BAD_REQUEST "<div class='container'><p class='error'>✗ Missing agreement field</p></div>" ctx
    }

// GUID validation - showcases validateFormField with parsing
let validateGuidHandler : WebPart =
    validateFormField "guid" (Parse.parseGuid "guid")
    >=> fun ctx ->
        let guid = getValidated<System.Guid> "guid" ctx |> Option.defaultValue System.Guid.Empty
        let response = sprintf $"""
            <div class="container">
                <p class="success">✓ Valid GUID!</p>
                <p><strong>GUID:</strong> {guid.ToString()}</p>
                <p><em>Using: validateFormField with Parse.parseGuid</em></p>
                <p><a href="/">← Back to examples</a></p>
            </div>
            """
        OK response ctx

// Tag validation endpoint (must be one of allowed values)
let validateTagHandler : WebPart =
    fun ctx -> async {
        match ctx.request.formData "tag" with
        | Choice1Of2 tag ->
            let allowedTags = ["bug"; "feature"; "docs"]
            match Compare.oneOf allowedTags "tag" tag with
            | Valid validTag ->
                let response = sprintf $"""
                    <div class="container">
                        <p class="success">✓ Valid tag!</p>
                        <p><strong>Tag:</strong> {validTag}</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! OK response ctx
            | Invalid errors ->
                let errorMsg = errors |> List.head
                let response = sprintf $"""
                    <div class="container">
                        <p class="error">✗ Tag validation failed</p>
                        <p><strong>Error:</strong> {errorMsg.message}</p>
                        <p><strong>Allowed tags:</strong> bug, feature, docs</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! BAD_REQUEST response ctx
        | Choice2Of2 _ ->
            return! BAD_REQUEST "<div class='container'><p class='error'>✗ Missing tag field</p></div>" ctx
    }

// Code pattern validation endpoint (must match ABC-1234 pattern)
let validateCodeHandler : WebPart =
    fun ctx -> async {
        match ctx.request.formData "code" with
        | Choice1Of2 code ->
            // Pattern: ABC-1234 (three uppercase letters, dash, four digits)
            let pattern = @"^[A-Z]{3}-\d{4}$"
            match String.matches pattern "code" "Code must match pattern ABC-1234" code with
            | Valid validCode ->
                let response = sprintf $"""
                    <div class="container">
                        <p class="success">✓ Valid code format!</p>
                        <p><strong>Code:</strong> {validCode}</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! OK response ctx
            | Invalid errors ->
                let errorMsg = errors |> List.head
                let response = sprintf $"""
                    <div class="container">
                        <p class="error">✗ Code validation failed</p>
                        <p><strong>Error:</strong> {errorMsg.message}</p>
                        <p><strong>Expected format:</strong> ABC-1234 (3 uppercase letters, dash, 4 digits)</p>
                        <p><a href="/">← Back to examples</a></p>
                    </div>
                    """
                return! BAD_REQUEST response ctx
        | Choice2Of2 _ ->
            return! BAD_REQUEST "<div class='container'><p class='error'>✗ Missing code field</p></div>" ctx
    }

[<EntryPoint>]
let main argv =
    printfn "==========================================="
    printfn "  Suave Input Validation Framework Demo"
    printfn "==========================================="
    printfn ""
    printfn "Server: http://localhost:8080"
    printfn ""
    printfn "This example demonstrates:"
    printfn "  ✓ Email validation"
    printfn "  ✓ Password strength validation"
    printfn "  ✓ Numeric range validation with parsing"
    printfn "  ✓ Alphanumeric validation"
    printfn "  ✓ Query parameter validation"
    printfn ""
    printfn "Press Ctrl+C to stop the server"
    printfn "==========================================="
    printfn ""
    
    let app =
        Writers.setMimeType "text/html; charset=utf-8" >=> 
        choose [
            GET >=> choose [ 
                        path "/" >=> OK htmlForm;
                        path "/search" >=> searchHandler
                    ];
            POST >=> choose [ 
                path "/validate-email" >=> validateEmailHandler;
                path "/validate-password" >=> validatePasswordHandler;
                path "/validate-age" >=> validateAgeHandler;
                path "/validate-username" >=> validateUsernameHandler;
                path "/validate-url" >=> validateUrlHandler;
                path "/validate-date" >=> validateDateHandler;
                path "/validate-price" >=> validatePriceHandler;
                path "/validate-bool" >=> validateBoolHandler;
                path "/validate-guid" >=> validateGuidHandler;
                path "/validate-tag" >=> validateTagHandler;
                path "/validate-code" >=> validateCodeHandler
                ]
        ]
    
    let config = { defaultConfig with bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 8080 ] }
    startWebServer config app
    0
