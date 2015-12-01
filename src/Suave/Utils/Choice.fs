module Suave.Utils.Choice

open System

let create x = Choice1Of2 x

let bindUnit f = function
  | Choice1Of2 x -> f x
  | Choice2Of2 _ -> Choice2Of2 ()

let ofOption onMissing = function
  | Some x -> Choice1Of2 x
  | None   -> Choice2Of2 onMissing

let orDefault onMissing = function
  | Choice1Of2 x -> x
  | Choice2Of2 _ -> onMissing

let iter f = function
  | Choice1Of2 x -> f x
  | Choice2Of2 _ -> ()

let parser (parse : string -> bool * 'a) (err : string) =
  fun original ->
    match parse original with
    | true, res -> Choice1Of2 res
    | _, _      -> Choice2Of2 (sprintf "%s. Input value \"%O\"" err original)

module Operators =

  let (|@) opt errorMsg =
    match opt with
    | Some x -> Choice1Of2 x
    | None -> Choice2Of2 errorMsg

  let (||@) c errorMsg =
    match c with
    | Choice1Of2 x -> Choice1Of2 x
    | Choice2Of2 y -> Choice2Of2 errorMsg