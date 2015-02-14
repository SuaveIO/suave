namespace Suave.Utils

module Option =
  let orDefault value opt =
    opt |> Option.fold (fun s t -> t) value
