module Suave.Utils.Option

let orDefault value opt =
  opt |> Option.fold (fun s t -> t) value

let ofChoice = function
  | Choice1Of2 x -> Some x
  | Choice2Of2 _ -> None