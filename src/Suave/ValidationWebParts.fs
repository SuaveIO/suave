namespace Suave

open System
open Suave.Validation

/// WebPart helpers for input validation
module ValidationWebParts =

  /// Extract and validate a form field
  let validateFormField (fieldName: string) (validator: Validator<string, 'T>) : WebPart =
    fun ctx -> async {
      match ctx.request.formData fieldName with
      | Choice1Of2 value ->
          match validator value with
          | Valid result ->
              ctx.userState.[fieldName] <- box result
              return Some ctx
          | Invalid errors ->
              let errorMsg = errors |> List.map (fun e -> e.message) |> String.concat "; "
              return! RequestErrors.BAD_REQUEST errorMsg ctx
      | Choice2Of2 _ ->
          return! RequestErrors.BAD_REQUEST (sprintf "Missing form field: %s" fieldName) ctx
    }

  /// Extract and validate a query parameter
  let validateQueryParam (paramName: string) (validator: Validator<string, 'T>) : WebPart =
    fun ctx -> async {
      match ctx.request.queryParam paramName with
      | Choice1Of2 value ->
          match validator value with
          | Valid result ->
              ctx.userState.[paramName] <- box result
              return Some ctx
          | Invalid errors ->
              let errorMsg = errors |> List.map (fun e -> e.message) |> String.concat "; "
              return! RequestErrors.BAD_REQUEST errorMsg ctx
      | Choice2Of2 _ ->
          return! RequestErrors.BAD_REQUEST (sprintf "Missing query parameter: %s" paramName) ctx
    }

  /// Extract and validate a header
  let validateHeader (headerName: string) (validator: Validator<string, 'T>) : WebPart =
    fun ctx -> async {
      match ctx.request.header headerName with
      | Choice1Of2 value ->
          match validator value with
          | Valid result ->
              ctx.userState.[headerName] <- box result
              return Some ctx
          | Invalid errors ->
              let errorMsg = errors |> List.map (fun e -> e.message) |> String.concat "; "
              return! RequestErrors.BAD_REQUEST errorMsg ctx
      | Choice2Of2 _ ->
          return! RequestErrors.BAD_REQUEST (sprintf "Missing header: %s" headerName) ctx
    }

  /// Validate multiple form fields
  type FormFieldValidator<'T> =
    { fieldName: string
      validator: Validator<string, obj>
      transform: obj -> 'T }

  /// Build a form validator that validates multiple fields and constructs a result
  let validateForm (fieldValidators: (string * Validator<string, 'T>) list) (constructor: obj list -> 'U) : WebPart =
    fun ctx -> async {
      let results =
        fieldValidators
        |> List.map (fun (fieldName, validator) ->
          match ctx.request.formData fieldName with
          | Choice1Of2 value ->
              match validator value with
              | Valid result -> Choice1Of2 (box result)
              | Invalid errors -> Choice2Of2 (fieldName, errors)
          | Choice2Of2 _ ->
              Choice2Of2 (fieldName, [errorSimple fieldName (sprintf "Missing field: %s" fieldName)]))
      
      let errors =
        results
        |> List.choose (function Choice2Of2 (field, errs) -> Some (field, errs) | _ -> None)
      
      if List.isEmpty errors then
        let values = results |> List.choose (function Choice1Of2 v -> Some v | _ -> None)
        let result = constructor values
        ctx.userState.["validatedForm"] <- box result
        return Some ctx
      else
        let errorMsg =
          errors
          |> List.collect (fun (field, errs) ->
            errs |> List.map (fun e -> sprintf "%s: %s" field e.message))
          |> String.concat "; "
        return! RequestErrors.BAD_REQUEST errorMsg ctx
    }

  /// Get a validated value from user state
  let getValidated<'T> (key: string) (ctx: HttpContext) : 'T option =
    match ctx.userState.TryGetValue(key) with
    | true, value -> Some (value :?> 'T)
    | false, _ -> None

  /// Validate request with custom validator and error handler
  let validateRequest (validator: HttpRequest -> ValidationResult<'T>) (onValid: 'T -> WebPart) (onInvalid: ValidationError list -> WebPart) : WebPart =
    fun ctx -> async {
      match validator ctx.request with
      | Valid result -> return! onValid result ctx
      | Invalid errors -> return! onInvalid errors ctx
    }

  /// Format validation errors as JSON
  let validationErrorsToJson (errors: ValidationError list) : string =
    let errorObjects =
      errors
      |> List.map (fun e ->
        let code = match e.code with Some c -> sprintf "\"code\": \"%s\"," c | None -> ""
        sprintf "{%s \"field\": \"%s\", \"message\": \"%s\"}" code e.field e.message)
      |> String.concat ","
    sprintf "{\"errors\": [%s]}" errorObjects

  // === Helper Functions ===

  /// Extract form data as a map
  let getFormMap (req: HttpRequest) : Map<string, string> =
    req.form
    |> List.choose (fun (k, v) -> v |> Option.map (fun value -> k, value))
    |> Map.ofList

  /// Extract query parameters as a map
  let getQueryMap (req: HttpRequest) : Map<string, string> =
    req.query
    |> List.choose (fun (k, v) -> v |> Option.map (fun value -> k, value))
    |> Map.ofList

  /// Validate a map of fields
  let validateMap (validators: Map<string, Validator<string, obj>>) (data: Map<string, string>) : ValidationResult<Map<string, obj>> =
    let results =
      validators
      |> Map.toList
      |> List.map (fun (key, validator) ->
        match Map.tryFind key data with
        | Some value ->
            match validator value with
            | Valid result -> Choice1Of2 (key, result)
            | Invalid errors -> Choice2Of2 errors
        | None ->
            Choice2Of2 [errorSimple key (sprintf "Missing field: %s" key)])
    
    let errors =
      results
      |> List.choose (function Choice2Of2 errs -> Some errs | _ -> None)
      |> List.concat
    
    if List.isEmpty errors then
      Valid (results |> List.choose (function Choice1Of2 (k, v) -> Some (k, v) | _ -> None) |> Map.ofList)
    else
      Invalid errors

  // === Middleware-style validators ===

  /// Create a middleware that validates and stores result in userState
  let middleware (key: string) (validator: HttpRequest -> ValidationResult<'T>) : WebPart =
    fun ctx -> async {
      match validator ctx.request with
      | Valid result ->
          ctx.userState.[key] <- box result
          return Some ctx
      | Invalid errors ->
          let errorMsg =
            errors
            |> List.map (fun e -> sprintf "%s: %s" e.field e.message)
            |> String.concat "; "
          return! RequestErrors.BAD_REQUEST errorMsg ctx
    }

  /// Combine multiple validation middlewares
  let validateAll (validators: (string * (HttpRequest -> ValidationResult<obj>)) list) : WebPart =
    fun ctx -> async {
      let results =
        validators
        |> List.map (fun (key, validator) ->
          match validator ctx.request with
          | Valid result -> Choice1Of2 (key, result)
          | Invalid errors -> Choice2Of2 errors)
      
      let errors =
        results
        |> List.choose (function Choice2Of2 errs -> Some errs | _ -> None)
        |> List.concat
      
      if List.isEmpty errors then
        results
        |> List.choose (function Choice1Of2 (k, v) -> Some (k, v) | _ -> None)
        |> List.iter (fun (k, v) -> ctx.userState.[k] <- v)
        return Some ctx
      else
        let errorMsg =
          errors
          |> List.map (fun e -> sprintf "%s: %s" e.field e.message)
          |> String.concat "; "
        return! RequestErrors.BAD_REQUEST errorMsg ctx
    }
