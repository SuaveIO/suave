namespace Suave.Http

/// This module contains the correct operator definitions.
module Monad =

  /// Concatenate f
  let inline (@@) a b = fun x ->
    match a x with
    | None   -> b x
    | r      -> r

