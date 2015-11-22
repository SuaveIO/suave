namespace Suave.Http

[<AutoOpen>]
module WebPart =

  open Operators

  /// Return success with some value
  val inline succeed : item:'T -> Async<'T option>

  /// Return failure without any value
  val fail : Async<HttpContext option> 

  /// Return failure with a value that is ignored
  val never : WebPart

  /// Entry-point for composing the applicative routes of the http application,
  /// by iterating the options, applying the context, arg, to the predicate
  /// from the list of options, until there's a match/a Some(x) which can be
  /// run.
  val choose : options : WebPart list -> WebPart

  /// Pipe the request through to a bird that can peck at it.
  ///
  /// Put another way, using 'warbler' lets you look at the first parameter and
  /// then make a decision about what thing to return (it's most likely a
  /// WebPart you'll be returning). (Remember, WebPart is
  /// HttpContext -> Async<HttpContext option>) where HttpContext is 'a and
  /// Async<_> is 'b.
  val inline warbler : f:('T -> 'T -> 'U) -> 'T -> 'U

  /// The constant function, which returns its constant, no matter
  /// its input.
  /// - theorem: identity = (cnst |> warbler)
  /// (warbler cnst) x = cnst x x = fun _ -> x
  val inline cnst : x:'T -> 'U -> 'T

  /// The conditional function that applies f x a if there's a value in d,
  /// or otherwise, applies g a, if there is no value in d.
  val cond : item:Choice<'T, _> -> f:('T -> 'U -> 'V) -> g:('U -> 'V) -> 'U -> 'V

  val inline tryThen : first:WebPart -> second:WebPart -> WebPart

  val inline concatenate : first:('a -> 'b option) -> second:('a -> 'b option) -> 'a -> 'b option

  module Operators =

    val inline (<|>) : first:WebPart -> second:WebPart -> WebPart
    val inline (@@)  : first:('a -> 'b option) -> second:('a -> 'b option) -> ('a -> 'b option)
