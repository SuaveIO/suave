namespace Suave

open System
open System.Text.RegularExpressions

/// Input Validation Framework for Suave
module Validation =

  /// Represents a validation error for a specific field
  type ValidationError =
    { field: string
      message: string
      code: string option }

  /// Result of a validation operation
  type ValidationResult<'T> =
    | Valid of 'T
    | Invalid of ValidationError list

  /// A validator that transforms input of type 'T to output of type 'U
  type Validator<'T, 'U> = 'T -> ValidationResult<'U>

  // === Core Validator Functions ===

  /// Create a validation error
  let error field message code =
    { field = field; message = message; code = code }

  /// Create a simple validation error without a code
  let errorSimple field message =
    error field message None

  /// Create a validator that always succeeds
  let succeed value : Validator<'T, 'U> =
    fun _ -> Valid value

  /// Create a validator that always fails
  let fail err : Validator<'T, 'U> =
    fun _ -> Invalid [err]

  /// Map over a valid value
  let map (f: 'U -> 'V) (validator: Validator<'T, 'U>) : Validator<'T, 'V> =
    fun input ->
      match validator input with
      | Valid value -> Valid (f value)
      | Invalid errors -> Invalid errors

  /// Bind validators sequentially (stop on first error)
  let bind (f: 'U -> Validator<'T, 'V>) (validator: Validator<'T, 'U>) : Validator<'T, 'V> =
    fun input ->
      match validator input with
      | Valid value -> f value input
      | Invalid errors -> Invalid errors

  /// Apply a validator that returns a function to a validator that returns a value
  let apply (validatorF: Validator<'T, 'U -> 'V>) (validatorX: Validator<'T, 'U>) : Validator<'T, 'V> =
    fun input ->
      match validatorF input, validatorX input with
      | Valid f, Valid x -> Valid (f x)
      | Invalid errs, Valid _ -> Invalid errs
      | Valid _, Invalid errs -> Invalid errs
      | Invalid errs1, Invalid errs2 -> Invalid (errs1 @ errs2)

  /// Compose two validators (passes input through both, collecting all errors)
  let andAlso (validator2: Validator<'T, 'U>) (validator1: Validator<'T, 'U>) : Validator<'T, 'U> =
    fun input ->
      match validator1 input, validator2 input with
      | Valid v, Valid _ -> Valid v
      | Invalid errs, Valid _ -> Invalid errs
      | Valid _, Invalid errs -> Invalid errs
      | Invalid errs1, Invalid errs2 -> Invalid (errs1 @ errs2)

  /// Try first validator, if it fails try second
  let orElse (validator2: Validator<'T, 'U>) (validator1: Validator<'T, 'U>) : Validator<'T, 'U> =
    fun input ->
      match validator1 input with
      | Valid _ as result -> result
      | Invalid _ -> validator2 input

  /// Make a validator optional (None is always valid)
  let optional (validator: Validator<'T, 'U>) : Validator<'T option, 'U option> =
    fun input ->
      match input with
      | None -> Valid None
      | Some value ->
          match validator value with
          | Valid v -> Valid (Some v)
          | Invalid errs -> Invalid errs

  /// Validate a list of items
  let list (validator: Validator<'T, 'U>) : Validator<'T list, 'U list> =
    fun input ->
      let results =
        input
        |> List.mapi (fun i item ->
          match validator item with
          | Valid v -> Valid v
          | Invalid errs ->
              Invalid (errs |> List.map (fun e -> { e with field = sprintf "%s[%d]" e.field i })))
      
      let errors =
        results
        |> List.choose (function Invalid errs -> Some errs | _ -> None)
        |> List.concat
      
      if List.isEmpty errors then
        Valid (results |> List.choose (function Valid v -> Some v | _ -> None))
      else
        Invalid errors

  // === Combinator Operators ===

  /// Infix operator for bind (>=>)
  let (>=>) = bind

  /// Infix operator for map (<^>)
  let (<^>) = map

  /// Infix operator for apply (<@>)
  let (<@>) f x = apply f x

  /// Infix operator for andAlso (<++>)
  let (<++>) = andAlso

  /// Infix operator for orElse (<//>)
  let (<//>) = orElse

  // === String Validators ===

  module String =
    
    /// Validate that a string is not null or empty
    let notEmpty field : Validator<string, string> =
      fun input ->
        if System.String.IsNullOrWhiteSpace(input) then
          Invalid [errorSimple field "Value cannot be empty"]
        else
          Valid input

    /// Validate minimum length
    let minLength (min: int) field : Validator<string, string> =
      fun input ->
        if input.Length < min then
          Invalid [errorSimple field (sprintf "Value must be at least %d characters" min)]
        else
          Valid input

    /// Validate maximum length
    let maxLength (max: int) field : Validator<string, string> =
      fun input ->
        if input.Length > max then
          Invalid [errorSimple field (sprintf "Value must be at most %d characters" max)]
        else
          Valid input

    /// Validate exact length
    let exactLength (length: int) field : Validator<string, string> =
      fun input ->
        if input.Length <> length then
          Invalid [errorSimple field (sprintf "Value must be exactly %d characters" length)]
        else
          Valid input

    /// Validate length range
    let lengthBetween (min: int) (max: int) field : Validator<string, string> =
      fun input ->
        match minLength min field input with
        | Invalid errs -> Invalid errs
        | Valid _ -> maxLength max field input

    /// Validate against a regex pattern
    let matches (pattern: string) field message : Validator<string, string> =
      fun input ->
        if Regex.IsMatch(input, pattern) then
          Valid input
        else
          Invalid [errorSimple field message]

    /// Validate email format
    let email field : Validator<string, string> =
      let pattern = @"^[^@\s]+@[^@\s]+\.[^@\s]+$"
      matches pattern field "Invalid email format"

    /// Validate URL format
    let url field : Validator<string, string> =
      fun input ->
        match Uri.TryCreate(input, UriKind.Absolute) with
        | true, uri when uri.Scheme = Uri.UriSchemeHttp || uri.Scheme = Uri.UriSchemeHttps ->
            Valid input
        | _ ->
            Invalid [errorSimple field "Invalid URL format"]

    /// Validate alphanumeric characters only
    let alphanumeric field : Validator<string, string> =
      matches @"^[a-zA-Z0-9]+$" field "Value must contain only letters and numbers"

    /// Validate alphabetic characters only
    let alpha field : Validator<string, string> =
      matches @"^[a-zA-Z]+$" field "Value must contain only letters"

    /// Validate numeric characters only
    let numeric field : Validator<string, string> =
      matches @"^[0-9]+$" field "Value must contain only numbers"

    /// Validate that string contains a value
    let contains (substring: string) field : Validator<string, string> =
      fun input ->
        if input.Contains(substring) then
          Valid input
        else
          Invalid [errorSimple field (sprintf "Value must contain '%s'" substring)]

    /// Validate that string starts with a prefix
    let startsWith (prefix: string) field : Validator<string, string> =
      fun input ->
        if input.StartsWith(prefix) then
          Valid input
        else
          Invalid [errorSimple field (sprintf "Value must start with '%s'" prefix)]

    /// Validate that string ends with a suffix
    let endsWith (suffix: string) field : Validator<string, string> =
      fun input ->
        if input.EndsWith(suffix) then
          Valid input
        else
          Invalid [errorSimple field (sprintf "Value must end with '%s'" suffix)]

  // === Numeric Validators ===

  module Numeric =
    
    /// Validate minimum value
    let min (minValue: 'T) field : Validator<'T, 'T> when 'T : comparison =
      fun input ->
        if input < minValue then
          Invalid [errorSimple field (sprintf "Value must be at least %A" minValue)]
        else
          Valid input

    /// Validate maximum value
    let max (maxValue: 'T) field : Validator<'T, 'T> when 'T : comparison =
      fun input ->
        if input > maxValue then
          Invalid [errorSimple field (sprintf "Value must be at most %A" maxValue)]
        else
          Valid input

    /// Validate value is within range (inclusive)
    let between (minValue: 'T) (maxValue: 'T) field : Validator<'T, 'T> when 'T : comparison =
      fun input ->
        match min minValue field input with
        | Invalid errs -> Invalid errs
        | Valid _ -> max maxValue field input

    /// Validate positive integer
    let positiveInt field : Validator<int, int> =
      fun input ->
        if input > 0 then Valid input
        else Invalid [errorSimple field "Value must be positive"]

    /// Validate non-negative integer
    let nonNegativeInt field : Validator<int, int> =
      fun input ->
        if input >= 0 then Valid input
        else Invalid [errorSimple field "Value must be non-negative"]

    /// Validate positive float
    let positiveFloat field : Validator<float, float> =
      fun input ->
        if input > 0.0 then Valid input
        else Invalid [errorSimple field "Value must be positive"]

    /// Validate non-negative float
    let nonNegativeFloat field : Validator<float, float> =
      fun input ->
        if input >= 0.0 then Valid input
        else Invalid [errorSimple field "Value must be non-negative"]

    /// Validate positive decimal
    let positiveDecimal field : Validator<decimal, decimal> =
      fun input ->
        if input > 0m then Valid input
        else Invalid [errorSimple field "Value must be positive"]

    /// Validate non-negative decimal
    let nonNegativeDecimal field : Validator<decimal, decimal> =
      fun input ->
        if input >= 0m then Valid input
        else Invalid [errorSimple field "Value must be non-negative"]

  // === Collection Validators ===

  module Collection =
    
    /// Validate that collection is not empty
    let notEmpty field : Validator<'T list, 'T list> =
      fun input ->
        if List.isEmpty input then
          Invalid [errorSimple field "Collection cannot be empty"]
        else
          Valid input

    /// Validate minimum count
    let minCount (min: int) field : Validator<'T list, 'T list> =
      fun input ->
        if input.Length < min then
          Invalid [errorSimple field (sprintf "Collection must have at least %d items" min)]
        else
          Valid input

    /// Validate maximum count
    let maxCount (max: int) field : Validator<'T list, 'T list> =
      fun input ->
        if input.Length > max then
          Invalid [errorSimple field (sprintf "Collection must have at most %d items" max)]
        else
          Valid input

    /// Validate exact count
    let exactCount (count: int) field : Validator<'T list, 'T list> =
      fun input ->
        if input.Length <> count then
          Invalid [errorSimple field (sprintf "Collection must have exactly %d items" count)]
        else
          Valid input

    /// Validate all items are unique
    let unique field : Validator<'T list, 'T list> when 'T : equality =
      fun input ->
        let distinctCount = input |> List.distinct |> List.length
        if distinctCount = input.Length then
          Valid input
        else
          Invalid [errorSimple field "Collection must contain only unique items"]

  // === Comparison Validators ===

  module Compare =
    
    /// Validate equality
    let equals (expected: 'T) field : Validator<'T, 'T> when 'T : equality =
      fun input ->
        if input = expected then
          Valid input
        else
          Invalid [errorSimple field (sprintf "Value must equal %A" expected)]

    /// Validate inequality
    let notEquals (forbidden: 'T) field : Validator<'T, 'T> when 'T : equality =
      fun input ->
        if input <> forbidden then
          Valid input
        else
          Invalid [errorSimple field (sprintf "Value must not equal %A" forbidden)]

    /// Validate value is in a list
    let oneOf (values: 'T list) field : Validator<'T, 'T> when 'T : equality =
      fun input ->
        if List.contains input values then
          Valid input
        else
          Invalid [errorSimple field "Value must be one of the allowed values"]

    /// Validate value is not in a list
    let noneOf (values: 'T list) field : Validator<'T, 'T> when 'T : equality =
      fun input ->
        if not (List.contains input values) then
          Valid input
        else
          Invalid [errorSimple field "Value must not be one of the forbidden values"]

  // === Date/Time Validators ===

  module DateTime =
    
    /// Validate date is in the future
    let future field : Validator<DateTime, DateTime> =
      fun input ->
        if input > DateTime.Now then
          Valid input
        else
          Invalid [errorSimple field "Date must be in the future"]

    /// Validate date is in the past
    let past field : Validator<DateTime, DateTime> =
      fun input ->
        if input < DateTime.Now then
          Valid input
        else
          Invalid [errorSimple field "Date must be in the past"]

    /// Validate date is before another date
    let before (maxDate: DateTime) field : Validator<DateTime, DateTime> =
      fun input ->
        if input < maxDate then
          Valid input
        else
          Invalid [errorSimple field (sprintf "Date must be before %s" (maxDate.ToString("yyyy-MM-dd")))]

    /// Validate date is after another date
    let after (minDate: DateTime) field : Validator<DateTime, DateTime> =
      fun input ->
        if input > minDate then
          Valid input
        else
          Invalid [errorSimple field (sprintf "Date must be after %s" (minDate.ToString("yyyy-MM-dd")))]

    /// Validate date is within range
    let between (minDate: DateTime) (maxDate: DateTime) field : Validator<DateTime, DateTime> =
      fun input ->
        match after minDate field input with
        | Invalid errs -> Invalid errs
        | Valid _ -> before maxDate field input

  // === Parsing Validators ===

  module Parse =
    
    /// Parse string to int
    let parseInt field : Validator<string, int> =
      fun input ->
        match System.Int32.TryParse(input) with
        | true, value -> Valid value
        | false, _ -> Invalid [errorSimple field "Value must be a valid integer"]

    /// Parse string to float
    let parseFloat field : Validator<string, float> =
      fun input ->
        match System.Double.TryParse(input) with
        | true, value -> Valid value
        | false, _ -> Invalid [errorSimple field "Value must be a valid number"]

    /// Parse string to decimal
    let parseDecimal field : Validator<string, decimal> =
      fun input ->
        match System.Decimal.TryParse(input) with
        | true, value -> Valid value
        | false, _ -> Invalid [errorSimple field "Value must be a valid decimal"]

    /// Parse string to bool
    let parseBool field : Validator<string, bool> =
      fun input ->
        match System.Boolean.TryParse(input) with
        | true, value -> Valid value
        | false, _ -> Invalid [errorSimple field "Value must be true or false"]

    /// Parse string to DateTime
    let parseDateTime field : Validator<string, DateTime> =
      fun input ->
        match System.DateTime.TryParse(input) with
        | true, value -> Valid value
        | false, _ -> Invalid [errorSimple field "Value must be a valid date/time"]

    /// Parse string to Guid
    let parseGuid field : Validator<string, Guid> =
      fun input ->
        match System.Guid.TryParse(input) with
        | true, value -> Valid value
        | false, _ -> Invalid [errorSimple field "Value must be a valid GUID"]

  // === Custom Validators ===

  /// Create a custom validator with a predicate function
  let custom (predicate: 'T -> bool) field message : Validator<'T, 'T> =
    fun input ->
      if predicate input then
        Valid input
      else
        Invalid [errorSimple field message]

  /// Create a custom validator that can return a specific error
  let customWithError (f: 'T -> Choice<'U, ValidationError>) : Validator<'T, 'U> =
    fun input ->
      match f input with
      | Choice1Of2 value -> Valid value
      | Choice2Of2 err -> Invalid [err]

  // === Async Validators ===

  /// Async validator type
  type AsyncValidator<'T, 'U> = 'T -> Async<ValidationResult<'U>>

  /// Convert sync validator to async
  let toAsync (validator: Validator<'T, 'U>) : AsyncValidator<'T, 'U> =
    fun input -> async { return validator input }

  /// Map over async validator
  let mapAsync (f: 'U -> 'V) (validator: AsyncValidator<'T, 'U>) : AsyncValidator<'T, 'V> =
    fun input -> async {
      let! result = validator input
      return
        match result with
        | Valid value -> Valid (f value)
        | Invalid errors -> Invalid errors
    }

  /// Bind async validators
  let bindAsync (f: 'U -> AsyncValidator<'T, 'V>) (validator: AsyncValidator<'T, 'U>) : AsyncValidator<'T, 'V> =
    fun input -> async {
      let! result = validator input
      return!
        match result with
        | Valid value -> f value input
        | Invalid errors -> async { return Invalid errors }
    }
