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

let binding = ChoiceBuilder()

[<AutoOpen>]
module Binding =

  open Suave

  let bind fBind
           (fCont : 'a -> (HttpContext -> 'c))
           (fErr  : 'b -> (HttpContext -> 'c))
           : (HttpContext -> 'c) =
    context (fun c ->
      match fBind c with
      | Choice1Of2 m   -> fCont m
      | Choice2Of2 err -> fErr err)

  let bindReq f fCont fErr =
    bind (fun ctx -> f ctx.request) fCont fErr

  let header key f (req : HttpRequest) =
    req.header key
    |> Choice.mapSnd (fun _ -> "Missing header '" + key + "'")
    |> Choice.bind f

  let form formKey f (req : HttpRequest) =
    req.formData formKey
    |> Choice.mapSnd (fun _ -> "Missing form field '" + formKey + "'")
    |> Choice.bind f

  let query queryKey f (req : HttpRequest) =
    req.queryParam queryKey
    |> Choice.mapSnd (fun _ -> "Missing query string key '" + queryKey + "'")
    |> Choice.bind f