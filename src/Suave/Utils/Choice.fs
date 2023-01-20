module Suave.Utils.Choice

let create x = Choice1Of2 x

let bindUnit f = function
  | Choice1Of2 x -> f x
  | Choice2Of2 _ -> Choice2Of2 ()

let ofOption onMissing = function
  | Some x -> Choice1Of2 x
  | None   -> Choice2Of2 onMissing

let toOption = function
  | Choice1Of2 x -> Some x
  | Choice2Of2 _ -> None

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
    | _, _      -> Choice2Of2 (err + ". Input value '" + original + "'" )

let createSnd v = Choice2Of2 v

let map f = function
  | Choice1Of2 v -> Choice1Of2 (f v)
  | Choice2Of2 msg -> Choice2Of2 msg

let mapSnd f = function
  | Choice1Of2 v -> Choice1Of2 v
  | Choice2Of2 v -> Choice2Of2 (f v)

let map2 f1 f2: Choice<'a, 'b> -> Choice<'c, 'd> = function
  | Choice1Of2 v -> Choice1Of2 (f1 v)
  | Choice2Of2 v -> Choice2Of2 (f2 v)

let bind (f: 'a -> Choice<'b, 'c>) (v: Choice<'a, 'c>) =
  match v with
  | Choice1Of2 v -> f v
  | Choice2Of2 c -> Choice2Of2 c

let bindSnd (f: 'a -> Choice<'c, 'b>) (v: Choice<'c, 'a>) =
  match v with
  | Choice1Of2 x -> Choice1Of2 x
  | Choice2Of2 x -> f x

let fold f g =
  function
  | Choice1Of2 x -> f x
  | Choice2Of2 y -> g y

let apply f v =
  bind (fun f' ->
    bind (fun v' ->
      create (f' v')) v) f

let applySnd f v =
  bind (fun f' ->
    bindSnd (fun v' ->
      createSnd (f' v')) v) f

let lift2 f v1 v2 =
  apply (apply (create f) v1) v2

let lift3 f v1 v2 v3 =
  apply (apply (apply (create f) v1) v2) v3

let lift4 f v1 v2 v3 v4 =
  apply (apply (apply (apply (create f) v1) v2) v3) v4

let lift5 f v1 v2 v3 v4 v5 =
  apply (apply (apply (apply (apply (create f) v1) v2) v3) v4) v5

let ofResult = function
  | Ok x -> Choice1Of2 x
  | Error x -> Choice2Of2 x

let toResult = function
  | Choice1Of2 x -> Ok x
  | Choice2Of2 x -> Error x

let inject f = function
  | Choice1Of2 x -> f x; Choice1Of2 x
  | Choice2Of2 x -> Choice2Of2 x

let injectSnd f = function
  | Choice1Of2 x -> Choice1Of2 x
  | Choice2Of2 x -> f x; Choice2Of2 x

module Operators =

  let (|@) opt errorMsg =
    match opt with
    | Some x -> Choice1Of2 x
    | None -> Choice2Of2 errorMsg

  let (||@) c errorMsg =
    match c with
    | Choice1Of2 x -> Choice1Of2 x
    | Choice2Of2 y -> Choice2Of2 errorMsg