[<System.Obsolete "for more info see https://github.com/SuaveIO/suave/releases/tag/v1.0.0 – this is a module to help the transition is not part of 'stable' v1.0 API as in semver">]
module Suave.Classic

open System
open System.Runtime.CompilerServices

[<Obsolete "Use Suave.WebPart.compose or >=> in Suave.Operators (exchanged with bind/>=>d), and for more info see https://github.com/SuaveIO/suave/releases/tag/v1.0.0">]
let inline bind (second : 'b -> Async<'c option>) (first : 'a -> Async<'b option>) : 'a -> Async<'c option> =
  WebPart.compose first second

[<Obsolete "Use Suave.WebPart.compose or >=> in Suave.Operators (exchanged with bind/>=>), and for more info see https://github.com/SuaveIO/suave/releases/tag/v1.0.0">]
let inline (>>=) (first : 'a -> Async<'b option>)  (second : 'b -> Async<'c option>) : 'a -> Async<'c option> =
  fun x ->
    WebPart.compose first second x

[<Obsolete "Use Suave.WebPart.bind or >>= in Suave.Operators (exchanged with compose/>=>), and for more info see https://github.com/SuaveIO/suave/releases/tag/v1.0.0">]
let inline (>=>) a b = fun x ->
  match a x with
  | None   -> b x
  | r      -> r

module Suave =
  [<Obsolete "Replace 'open Suave.Auth' with 'open Suave.Authentication'">]
  module Auth =
    ()
  [<Obsolete "Replace 'open Suave.Types' with 'open Suave.Http' or 'open Suave'">]
  module Types =
    ()
  module Http =
    [<Obsolete "Replace 'open Suave.Http.Response' with 'open Suave.Response'">]
    module Response =
      ()
    [<Obsolete "Replace 'open Suave.Http.Intermediate' with 'open Suave.Intermediate'">]
    module Intermediate =
      ()
    [<Obsolete "Replace 'open Suave.Http.Successful' with 'open Suave.Successful'">]
    module Successful =
      ()
    [<Obsolete "Replace 'open Suave.Http.Redirection' with 'open Suave.Redirection'">]
    module Redirection =
      ()
    [<Obsolete "Replace 'open Suave.Http.RequestErrors' with 'open Suave.RequestErrors'">]
    module RequestErrors =
      ()
    [<Obsolete "Replace 'open Suave.Http.ServerErrors' with 'open Suave.ServerErrors'">]
    module ServerErrors =
      ()
    [<Obsolete "Replace 'open Suave.Http.Applicatives' with 'open Suave.Filters'">]
    module Applicatives =
      ()
    [<Obsolete "Let us know at https://github.com/SuaveIO/suave/issues if you were using this module and for what. Currently an internal module.">]
    module ServeResource =
      ()
    [<Obsolete "Replace 'open Suave.Http.Files' with 'open Suave.Files'">]
    module Files =
      ()
    [<Obsolete "Replace 'open Suave.Http.Embedded' with 'open Suave.Embedded'">]
    module Embedded =
      ()
    [<Obsolete "Replace 'open Suave.Http.EventSource' with 'open Suave.EventSource'">]
    module EventSource =
      ()
    [<Obsolete "Replace 'open Suave.Http.Authentication' with 'open Suave.Authentication'">]
    module Authentication =
      ()
    [<Obsolete "Replace 'open Suave.Http.Control' with 'open Suave.Control'">]
    module Control =
      ()
