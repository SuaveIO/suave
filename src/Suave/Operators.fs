namespace Suave

module Operators =

  let (>>=) a b = WebPart.bind b a

  let (>=>) a b = WebPart.compose a b

  let inline (<|>) a b = WebPart.tryThen a b

  let inline (@@) a b = WebPart.concatenate a b
