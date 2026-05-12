module Suave.Utils.Option

  open System

  let create x = Some x

  let apply (f : ('a -> 'b) option) (v: 'a option) =
    Option.bind (fun f' ->
      Option.bind (fun v' ->
        create (f' v')) v) f

  let lift2 f v1 v2 =
    apply (apply (create f) v1) v2

  let lift3 f v1 v2 v3 =
    apply (apply (apply (create f) v1) v2) v3

  let lift4 f v1 v2 v3 v4 =
    apply (apply (apply (apply (create f) v1) v2) v3) v4

  let lift5 f v1 v2 v3 v4 v5 =
    apply (apply (apply (apply (apply (create f) v1) v2) v3) v4) v5

  let ofChoice = function
    | Choice1Of2 x -> Some x
    | _ -> None

  let toChoice case2 = function
    | Some x -> Choice1Of2 x
    | None   -> Choice2Of2 (case2 ())

  let ofNullable nullable: 'a option =
    match box nullable with
    | null -> None // CLR null
    | :? Nullable<_> as n when not n.HasValue -> None // CLR struct
    | :? Nullable<_> as n when n.HasValue -> Some (n.Value) // CLR struct
    | x when x.Equals (DBNull.Value) -> None // useful when reading from the db into F#
    | x -> Some (unbox x) // anything else

  let toNullable = function
    | Some item -> new Nullable<_>(item)
    | None      -> new Nullable<_>()

  let orDefault x = function
    | None -> x ()
    | Some y -> y

  let inject f = function
    | Some x -> f x; Some x
    | None   -> None

  module Operators =

    let inline (>>=) m f =
      Option.bind f m

    let inline (=<<) f m =
      Option.bind f m

    let inline (>>*) m f =
      inject f m

    let inline (<*>) f m =
      apply f m

    let inline (<!>) f m =
      Option.map f m

    let inline ( *>) m1 m2 =
      lift2 (fun _ x -> x) m1 m2

    let inline ( <*) m1 m2 =
      lift2 (fun x _ -> x) m1 m2

