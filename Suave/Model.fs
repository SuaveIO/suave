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
  open System.Globalization

  let private parse_using<'a> (f:string -> bool * 'a) s =
    match f s with
    | true, i -> Choice1Of2 i
    | false, _ -> Choice2Of2 (sprintf "Cound not parse '%s' to %s" s typeof<'a>.Name)

  let int32 = parse_using Int32.TryParse
  let uint32 = parse_using UInt32.TryParse
  let int64 = parse_using Int64.TryParse
  let uint64 = parse_using UInt64.TryParse
  let uri = parse_using (fun s -> Uri.TryCreate(s, UriKind.RelativeOrAbsolute))
  let date_time = parse_using (fun s -> DateTime.TryParse(s, CultureInfo.InvariantCulture.DateTimeFormat, DateTimeStyles.RoundtripKind))

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
