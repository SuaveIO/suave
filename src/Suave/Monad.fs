namespace Suave.Http

/// This module contains the correct operator definitions.
module Monad =

  





  /// Concatenate f
  let inline (@@) a b = fun x ->
    match a x with
    | None   -> b x
    | r      -> r

  let inline (<|>) (a : WebPart) (b : WebPart) : WebPart =
    fun x ->
      async {
        let! e = a x
        match e with
        | None ->
          let! result = b x
          match result with
          | None -> return None
          | r -> return r
        | r -> return r
      }