namespace Suave.Utils

module Option =
  let or_default value opt =
    opt |> Option.fold (fun s t -> t) value
