module Suave.Classic

open System
open System.Runtime.CompilerServices

[<Obsolete "Use Suave.WebPart.bind">]
let inline bind (second : 'b -> Async<'c option>) (first : 'a -> Async<'b option>) : 'a -> Async<'c option> =
  WebPart.compose first second

let inline (>>=) (first : 'a -> Async<'b option>)  (second : 'b -> Async<'c option>) : 'a -> Async<'c option> =
  fun x ->
    WebPart.compose second first x

let inline (>=>) a b = fun x ->
  match a x with
  | None   -> b x
  | r      -> r