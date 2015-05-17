module Suave.Form

open Microsoft.FSharp.Reflection
open Microsoft.FSharp.Quotations
open Microsoft.FSharp.Quotations.Patterns

open System
open System.Net.Mail
open System.Text.RegularExpressions

open Suave.Html
open Suave.Utils
open Suave.Types
open Suave.Model

type MailAddress with
  static member Create s =
    try
      Choice1Of2 (MailAddress(s))
    with :? FormatException ->
      Choice2Of2 (sprintf "%s is not a valid email" s)

type Password = Password of string

type ServerSideMsg = string
type HtmlAttribute = string * string
type Validation<'a> = ('a -> bool) * ServerSideMsg * HtmlAttribute
type ServerSideValidation<'a> = ('a -> bool) * ServerSideMsg

type Property<'a, 'b> = ('a -> Expr<'b>) * Validation<'b> list

type FormProp<'a> =
  | TextProp of Property<'a, string>
  | PasswordProp of Property<'a, Password>
  | DecimalProp of Property<'a, decimal>

type Form<'a> = Form of FormProp<'a> list * ServerSideValidation<'a> list

let formatDec (d : Decimal) = d.ToString(Globalization.CultureInfo.InvariantCulture)

let (|Optional|_|) (typ : Type) = 
  if typ.IsGenericType 
     && typ.GetGenericTypeDefinition() = typedefof<option<_>> then 
    Some(typ.GetGenericArguments().[0])
  else None
  
let private parse = function
| Optional(t), "" -> Choice1Of2 None |> Choice.map box
| Optional(genT), value -> 
  match genT, value with
  | t, value when t = typeof<String> -> Choice1Of2 value |> Choice.map (Some >> box)
  | t, value when t = typeof<Password> -> Password value |> Choice1Of2 |> Choice.map (Some >> box)
  | t, value when t = typeof<Decimal> -> Suave.Model.Parse.decimal value |> Choice.map (Some >> box)
  | t, value when t = typeof<MailAddress> -> MailAddress.Create value |> Choice.map (Some >> box)
  | t, _ -> failwithf "not supported type: %s" t.FullName

| t, value when t = typeof<String> -> Choice1Of2 value |> Choice.map box
| t, value when t = typeof<Password> -> Password value |> Choice1Of2 |> Choice.map box
| t, value when t = typeof<Decimal> -> Suave.Model.Parse.decimal value |> Choice.map box
| t, value when t = typeof<MailAddress> -> MailAddress.Create value |> Choice.map box
| t, _ -> failwithf "not supported type: %s" t.FullName

let private validateSingle ((quotF, ((test, msg, _) : Validation<'b>)), value : 'a) =
  match quotF value with
  | PropertyGet (Some (Value (:? 'a as v, _)), p, _) -> 
    let propVal = p.GetGetMethod().Invoke(v, [||]) :?> 'b
    if test propVal then Choice1Of2 value
    else Choice2Of2 (sprintf "%s %s" p.Name msg)
  | _ -> failwith "unrecognized quotation"

let private getName (quotF : 'a -> Expr<'b>) =
  let n = Unchecked.defaultof<'a>
  match quotF n with
  | PropertyGet (_, p, _) -> 
    p.Name
  | _ -> failwith "unrecognized quotation"

let private validate ((quotF, validations), value : 'a) =
  validations
  |> List.fold
    (fun value validation ->
      value |> Choice.bind (fun value -> validateSingle ((quotF, validation), value)))
    (Choice1Of2 value)  

let private getQuotName = function 
  | TextProp (q, _) -> getName q
  | PasswordProp (q, _) -> getName q
  | DecimalProp (q, _) -> getName q

let private validate' = function
  | TextProp p, v -> validate (p, v)
  | PasswordProp p, v -> validate (p, v)
  | DecimalProp p, v -> validate (p, v)

let bindForm<'a> (form : Form<'a>) (req : HttpRequest) =
  let bindForm key = Model.Binding.form key Choice1Of2
  let t = form.GetType().GetGenericArguments().[0]
  let props = t.GetProperties() |> Array.toList
  let types = props |> List.map (fun p -> p.PropertyType)

  let getValue (prop : Reflection.PropertyInfo) =
    match prop.PropertyType with
    | Optional(_) -> defaultArg (req.formData prop.Name) "" |> Choice1Of2
    | _ -> req |> bindForm prop.Name

  let bindFold s f = List.fold (fun x next -> x |> Choice.bind (f next)) (Choice1Of2 s)
  let bindMap f = bindFold [] (fun x xs -> f x |> Choice.map (fun x -> x :: xs)) >> Choice.map List.rev

  binding {
    let! values = props |> bindMap getValue
    let! recordFields = 
      (types, values) 
      ||> List.zip
      |> bindMap parse

    let record = FSharpValue.MakeRecord(t, Array.ofList recordFields) :?> 'a
    let (Form (props,validations)) = form
    let! record =
      props
      |> bindFold record (fun prop record -> validate' (prop, record))
    let! record =
      validations
      |> bindFold 
        record 
        (fun (validation, msg) record -> 
          if validation record 
          then Choice1Of2 record 
          else Choice2Of2 msg)

    return record
  }

let maxLength max : Validation<string> = 
  (fun s -> s.Length <= max), 
  (sprintf "must be at most %d characters" max), 
  ("maxlength", max.ToString())

let matches pattern : Validation<string> =
  (fun p -> Regex(pattern).IsMatch(p)),
  (sprintf "doesn't match pattern %s" pattern),
  ("pattern", pattern)

let passwordRegex pattern : Validation<Password> =
  (fun (Password p) -> Regex(pattern).IsMatch(p)),
  (sprintf "doesn't match pattern %s" pattern),
  ("pattern", pattern)

let min min : Validation<decimal> =
  (fun d -> d >= min),
  (sprintf "must be at least %M" min),
  ("min", formatDec min)

let max max : Validation<decimal> =
  (fun d -> d <= max),
  (sprintf "must be at most %M" max),
  ("max", formatDec max)

let step step : Validation<decimal> = 
  (fun d -> d % step = 0.0M), 
  (sprintf "must be a multiply of %M" step), 
  ("step", formatDec step)


// HTML Markup

let option value txt selected =
  if selected then
    tag "option" ["value", value; "selected", "selected"] (text txt)
  else
    tag "option" ["value", value] (text txt)

let private inputType = function
  | t when t = typeof<String> -> "text"
  | t when t = typeof<Password> -> "password"
  | t when t = typeof<MailAddress> -> "email"
  | t when t = typeof<Decimal> -> "number"
  | t -> failwithf "not supported type: %s" t.FullName

let inline private thrd (_,_,x) = x

let private getHtmlAttrs = function
  | TextProp (_,xs) -> xs |> List.map thrd
  | PasswordProp (_, xs) -> xs |> List.map thrd
  | DecimalProp (_,xs) -> xs |> List.map thrd

let private getHtmlProps (Form (props,_)) (quotF : 'a -> Expr<'b>) : (string * string) list =
  props
  |> List.filter (fun p -> getName quotF = (p |> getQuotName))
  |> List.collect (fun p -> p |> getHtmlAttrs)

let input<'a, 'b> (quotF : 'a -> Expr<'b>) attrs (form : Form<'a>) =
  let name = getName quotF
  let typ = 
    match typeof<'b> with
    | Optional(t) 
    | t -> inputType t 
  let required = 
    match typeof<'b> with
    | Optional(_) -> []
    | _ -> ["required",""]
  let props = getHtmlProps form quotF
  inputAttr (["name", name; "type", typ]
        @ required
        @ attrs
        @ props)

let private format : (obj -> string) = function
  | :? string as s -> s
  | :? int as i -> string i
  | :? decimal as d -> formatDec d
  | t -> failwithf "unsupported type: %s" (t.GetType().FullName)

let selectInput<'a, 'b when 'b : equality> 
    (quotF : 'a -> Expr<'b>) 
    (options : ('b * string) list)
    (selected : 'b option) 
    (form : Form<'a>) =
  let x = 
    options
    |> List.map (fun (value,txt) -> option (format value) txt (selected = Some value))

  tag "select" ["name", getName quotF] (flatten x)


(*

example: 

type Register = {
  Username : string
  Email : MailAddress
  Password : Password
  ConfirmPassword : Password
}

let pattern = @"(\w){6,20}"

let passwordsMatch = 
  (fun f -> f.Password = f.ConfirmPassword), "Passwords must match"

let register : Form<Register> = 
  Form ([ TextProp ((fun f -> <@ f.Username @>), [ maxLength 30 ] )
      PasswordProp ((fun f -> <@ f.Password @>), [ passwordRegex pattern ] )
      PasswordProp ((fun f -> <@ f.ConfirmPassword @>), [ passwordRegex pattern ] )
      ],[ passwordsMatch ])


Handler:
Binding.bindReq (bindForm register) handler BAD_REQUEST

HTML markup:
input (fun f -> <@ f.Password @>) [] register

*)