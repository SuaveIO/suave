[<AutoOpen>]
module Suave.WebPart
/// Takes 'a and returns SuaveTask of 'a
/// SuaveTask is also known as AsyncOption
type WebPart<'a> = 'a -> Async<'a option>

val inline succeed : WebPart<'a>

val fail<'a>  : Async<'a option>

val never : WebPart<'a>

/// Classic bind (for SuaveTask)
val bind : f:('a -> Async<'b option>) -> a: Async<'a option> -> Async<'b option>

/// Left-to-right Kleisli composition (for SuaveTask).
val compose : first:('a -> Async<'b option>) -> second:('b -> Async<'c option>) ->  'a -> Async<'c option>

type AsyncOptionBuilder =
  new : unit -> AsyncOptionBuilder
  member Return : 'a -> Async<'a option>
  member Zero : unit -> Async<unit option>
  member ReturnFrom : Async<'a option> -> Async<'a option>
  member Delay : (unit ->  Async<'a option>) -> Async<'a option>
  member Bind : Async<'a option> * ('a -> Async<'b option>) ->  Async<'b option>

///  With this workflow you can write WebParts like this
///  let task ctx = asyncOption {
///    let! _ = GET ctx
///    let! ctx = Writers.setHeader "foo" "bar"
///    return ctx
///  }
///
///  we can still use the old symbol but now has a new meaning
///  let foo ctx = GET ctx >>= OK "hello"
///
val asyncOption : AsyncOptionBuilder

/// Entry-point for composing the applicative routes of the http application,
/// by iterating the options, applying the context, arg, to the predicate
/// from the list of options, until there's a match/a Some(x) which can be
/// run.
val choose : options:WebPart<'a> list -> WebPart<'a>

/// Inject a webPart
///
/// +------------+                                            +--------------+
/// | url "/a"   +----------+                       +---------+   cont1      |
/// +------------+          |                       |         +--------------+
///                         |                       |
/// +-------------+         |       +----------+    |         +--------------+
/// |  url "/b"   +---------+-------+ injected +----+---------+  cont2       |
/// +-------------+         |       +----------+    |         +--------------+
///                         |                       |
/// +-------------+         |                       |         +--------------+
/// | url "/b"    +---------+                       +---------+  cont3       |
/// +-------------+                                           +--------------+
val inject : postOp:WebPart<'a> -> pairs:(WebPart<'a> * WebPart<'a>) list -> WebPart<'a>

/// Which bird? A Warbler!
///
/// Pipe the request through to a bird that can peck at it.
///
/// Put another way, using 'warbler' lets you look at the first parameter and
/// then make a decision about what thing to return (it's most likely a
/// WebPart you'll be returning). (Remember, WebPart is
/// HttpContext -> Async<HttpContext option>) where HttpContext is 'a and
/// Async<_> is 'b.
val inline warbler : f:('t -> 't -> 'u) -> 't -> 'u

/// The constant function, which returns its constant, no matter
/// its input.
/// - theorem: identity = (warbler cnst)
/// (warbler cnst) x = cnst x x = fun _ -> x
val inline cnst : x:'t -> 'u -> 't

/// The conditional function that applies f x a if there's a value in d,
/// or otherwise, applies g a, if there is no value in d.
val cond : item:Choice<'T, _> -> f:('T -> 'U -> 'V) -> g:('U -> 'V) -> 'U -> 'V

val inline tryThen : first:WebPart<'a> -> second:WebPart<'a> -> WebPart<'a>

val inline concatenate : first:('a -> 'b option) -> second:('a -> 'b option)
                       -> 'a -> 'b option
