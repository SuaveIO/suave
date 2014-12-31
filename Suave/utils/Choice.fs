namespace Suave.Utils

module Choice =

  let mk x = Choice1Of2 x

  let map f = function
    | Choice1Of2 v   -> Choice1Of2 (f v)
    | Choice2Of2 err -> Choice2Of2 err

  let map_2 f = function
    | Choice1Of2 v   -> Choice1Of2 v
    | Choice2Of2 err -> Choice2Of2 (f err)

  let bind (f : 'a -> Choice<'b, 'c>) (v : Choice<'a, 'c>) =
    match v with
    | Choice1Of2 v -> f v
    | Choice2Of2 c -> Choice2Of2 c

  let from_option on_missing = function
    | Some x -> Choice1Of2 x
    | None   -> Choice2Of2 on_missing
