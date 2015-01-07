namespace Suave.Utils

module Map =

  let put key value m =
    match m |> Map.tryFind key with
    | None -> m |> Map.add key value
    | Some _ -> m |> Map.remove key |> Map.add key value