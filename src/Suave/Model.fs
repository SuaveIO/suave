module Suave.Model

open Suave.Utils

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

  let private parseUsing<'T> (f:string -> bool * 'T) s =
    match f s with
    | true, i -> Choice1Of2 i
    | false, _ -> Choice2Of2 (sprintf "Cound not parse '%s' to %s" s typeof<'T>.Name)

  let int32 = parseUsing Int32.TryParse
  let uint32 = parseUsing UInt32.TryParse
  let int64 = parseUsing Int64.TryParse
  let uint64 = parseUsing UInt64.TryParse
  let uri = parseUsing (fun s -> Uri.TryCreate(s, UriKind.RelativeOrAbsolute))
  let date_time = parseUsing (fun s -> DateTime.TryParse(s, CultureInfo.InvariantCulture.DateTimeFormat, DateTimeStyles.RoundtripKind))
  let decimal = parseUsing (fun s -> Decimal.TryParse(s, NumberStyles.AllowDecimalPoint, CultureInfo.InvariantCulture))

let binding = ChoiceBuilder()

[<AutoOpen>]
module Binding =

  open Suave
  open Suave.Types
  open Suave.Utils

  let bind f_bind
           (f_cont : 'a -> (HttpContext -> 'c))
           (f_err  : 'b -> (HttpContext -> 'c))
           : (HttpContext -> 'c) =
    context (fun c ->
      match f_bind c with
      | Choice1Of2 m   -> f_cont m
      | Choice2Of2 err -> f_err err)

  let bind_req f f_cont f_err =
    bind (HttpContext.request >> f) f_cont f_err

  let header key f (req : HttpRequest) =
    (req.headers %% key)
    |> Choice.from_option (sprintf "Missing header '%s'" key)
    |> Choice.bind f

  let form formKey f (req : HttpRequest) =
    req.formDataItem formKey
    |> Choice.from_option (sprintf "Missing form field '%s'" formKey)
    |> Choice.bind f

  let query queryKey f (req : HttpRequest) =
    req.queryParam queryKey
    |> Choice.from_option (sprintf "Missing query string key '%s'" queryKey)
    |> Choice.bind f
