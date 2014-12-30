namespace Suave.Utils

module List =

  let flat_map f xs =
    xs
    |> List.map f
    |> List.concat

