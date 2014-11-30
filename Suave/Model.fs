module Suave.Model

[<AutoOpen>]
module SyntacticSugar =

  let (>>.) a f = Choice.bind f a

type ChoiceBuilder() =
  member x.Bind (v, f) = Choice.bind f v
  member x.Return v = Choice1Of2 v
  member x.ReturnFrom o = o
  member x.Run f = f()
  member x.Combine (v, f:unit -> _) = Choice.bind f v
  member x.Delay (f : unit -> 'T) = f

[<RequireQualifiedAccess>]
module Parse =
  open System

  let string (str : string) =
    Choice1Of2 str

  let int32 str =
    match Int32.TryParse str with
    | true, i -> Choice1Of2 i
    | _       -> Choice2Of2 (sprintf "Could not parse '%s' to int" str)

  let uint32 str =
    match UInt32.TryParse str with
    | true, i -> Choice1Of2 i
    | _       -> Choice2Of2 (sprintf "Could not parse '%s' to int" str)

  let int64 str =
    match Int64.TryParse str with
    | true, i -> Choice1Of2 i
    | _       -> Choice2Of2 (sprintf "Could not parse '%s' to int" str)

  let uint64 str =
    match UInt64.TryParse str with
    | true, i -> Choice1Of2 i
    | _       -> Choice2Of2 (sprintf "Could not parse '%s' to int" str)

  let uri (s : string) =
    match Uri.TryCreate (s, UriKind.RelativeOrAbsolute) with
    | true, uri -> Choice1Of2 uri
    | false, _  -> Choice2Of2 (sprintf "Could not parse '%s' into uri" s)

let binding = ChoiceBuilder()

[<AutoOpen>]
module Binding =

  open Suave
  open Suave.Types

  let bind f_bind
           (f_cont : 'a -> (HttpContext -> 'c))
           (f_err  : 'b -> (HttpContext -> 'c))
           : (HttpContext -> 'c) =
    context (fun c ->
      match f_bind c with
      | Choice1Of2 m   -> f_cont m
      | Choice2Of2 err -> f_err err)

  let bind_req f_bind f_cont f_err =
    bind (HttpContext.request >> f_bind) f_cont f_err

  let header key f_bind (req : HttpRequest) =
    (req.headers %% key)
    |> Choice.from_option (sprintf "Missing header '%s'" key)
    |> Choice.bind f_bind

  let form form_key f_bind (req : HttpRequest) =
    (HttpRequest.form req) ^^ form_key
    |> Choice.from_option (sprintf "Missing form field '%s'" form_key)
    |> Choice.bind f_bind

  let query qs_key f_bind (req : HttpRequest) =
    (HttpRequest.query req) ^^ qs_key
    |> Choice.from_option (sprintf "Missing query string key '%s'" qs_key)
    |> Choice.bind f_bind
