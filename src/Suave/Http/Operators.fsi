namespace Suave.Http

module Operators =

  open Suave.Sockets

  /// Compose (bind) two arguments, 'first' and 'second', so that the result of
  /// the composition can be applied to an argument of 'input' and then passed
  /// to 'second', if 'first' yields a value.
  val inline (>>=) : first:('T -> Async<'U option>) -> second:('U -> Async<'V option>) -> input:'T -> Async<'V option>

  /// Compose (bind) two web parts; see (>>=) -- note the different parameter
  /// ordering
  val inline bind : second:('U -> Async<'V option>) -> first:('T -> Async<'U option>) -> input:'T -> Async<'V option>

  /// Left-to-right Kleisli composition of monads.
  val inline (>=>) : first:('T -> 'U option) -> second:('T -> 'U option) -> input:'T -> 'U option

  /// Left-to-right Kleisli composition of web parts.
  val inline (<|>) : first:WebPart -> second:WebPart -> WebPart

