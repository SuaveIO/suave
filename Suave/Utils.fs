/// A module for composing the applicatives.
[<AutoOpen>]
module Suave.Utils

open System.Collections.Generic

/// Return success with some value
let succeed x = Some(x)

/// Return failure without any value
let fail = None

/// Return failure with a value that is ignored
let never _ = None

/// bind f inp evaluates to match inp with None -> None | Some x -> f x
/// The same as Option.bind.
let bind = Option.bind

/// Delay the computation of f
let delay f = f()

/// Compose (bind) two arguments, 'a' and 'b', so that the result of
/// the composition can be applied to an argument of 'a' and then passed
/// to 'b', if 'a' yields a value.
let (>>=) a b = fun x -> bind b (a x)

/// Entry-point for composing the applicative routes of the http application,
/// by iterating the options, applying the context, arg, to the predicate
/// from the list of options, until there's a match/a Some(x) which can be
/// run.
let rec choose options arg =
  match options with
  | []        -> None
  | p :: tail ->
    match p arg with
    | Some x -> Some x
    | None   -> choose tail arg

/// Pipe the request through a bird that can peck at it.
let warbler f a = f a a //which bird? A Warbler!

/// Pipe the request through a bird that can peck at it.
let (>>==) a b = a >>= warbler (fun r -> b r)

/// Try find a value by key in a dictionary
let look_up (target : IDictionary<'b,'a>) key =
  match target.TryGetValue(key) with
  | true, v  -> Some v
  | false, _ -> None

/// Try find a value by key in a dictionary
let (?) (target : IDictionary<'b,'a>) key =
  look_up target key

/// Assign a value to the key in the dictionary
let (?<-) (target : IDictionary<string, 'a>) key value =
  target.[key] <- value

/// Force the evaluation of the option, so that if there is no value,
/// an InvalidOperationException is raised.
let opt = function
  | Some x -> x
  | None   -> invalidArg "arg1" "The argument was required but was not present"

/// The constant function, which returns its constant, no matter
/// its input.
let cnst x = fun _ -> x

/// The conditional function that applies f x a if there's a value in d,
/// or otherwise, applies g a, if there is no value in d.
let cond d f g a =
  match d with
  | Some x -> f x a
  | None   -> g a

//- theorem: identity = (cnst |> warbler)
//(warbler cnst) x = cnst x x = fun _ -> x

open System.IO

/// Fully transform the input stream to a byte array.
let read_fully (input:Stream) =
  use ms = new MemoryStream()
  input.CopyTo(ms)
  ms.ToArray()

open System
open System.Text

// TODO: http://greenbytes.de/tech/webdav/rfc6266.html#rfc.section.1.p.4%3E
// Test usage of these to ensure that ASCII is the correct encoding to use here.

/// Encode the string as ASCII encoded in Base64.
let encode_base64 (s : string) =
  let bytes = Encoding.ASCII.GetBytes s
  Convert.ToBase64String bytes

/// Decode the string containing Base64-encoded ASCII string data to
/// a .Net string
let decode_base64 (s : string) =
  let bytes = Convert.FromBase64String s
  Encoding.ASCII.GetString bytes

/// The end-of-line literal, \r\n (CRLF)
let [<Literal>] eol = "\r\n"

/// Get the ASCII bytes for the string
let bytes (s : string) =
  Encoding.ASCII.GetBytes s

/// Get the UTF-8 bytes for the string
let bytes_utf8 (s : string) =
  Encoding.UTF8.GetBytes s

/// The end-of-line 'literal' as bytes, the \r\n (CRLF) byte pair
let EOL = bytes eol

/// Write the string s to the stream asynchronously
/// as ASCII encoded text
let async_writeln (stream : Stream) s = async {
  let b = bytes s
  do! stream.AsyncWrite(b, 0, b.Length)
  do! stream.AsyncWrite(EOL, 0, 2)
}

/// Write the string s to the stream asynchronously
/// from a byte array
let async_writebytes (stream:Stream) b = async {
  do! stream.AsyncWrite(b, 0, b.Length)
}

/// Launch the function f on its own asynchronous/thread context
/// so that it doesn't block execution.
let unblock f =
  async {
    do! Async.SwitchToNewThread ()
    let res = f()
    do! Async.SwitchToThreadPool ()
    return res
  }

open System.Threading.Tasks

let awaitTask (t: Task) = t |> Async.AwaitIAsyncResult |> Async.Ignore

/// Asynchronouslyo write from the 'from' stream to the 'to' stream.
let transfer (to_stream : Stream) (from : Stream) =
    let buf = Array.zeroCreate<byte> 0x2000
    let rec doBlock () =
      async {
        let! read = from.AsyncRead buf
        if read <= 0 then
          to_stream.Flush()
          return ()
        else
          do! to_stream.AsyncWrite(buf, 0, read)
          return! doBlock () }
    doBlock ()

/// Knuth-Morris-Pratt algorithm
/// http://caml.inria.fr/pub/old_caml_site/Examples/oc/basics/kmp.ml
let init_next p =
  let m = Array.length p
  let next = Array.create m 0
  let i = ref 1
  let j = ref 0
  while !i < m - 1 do
    if p.[!i] = p.[!j] then begin incr i; incr j; next.[!i] <- !j end else
    if !j = 0 then begin incr i; next.[!i] <- 0 end else j := next.[!j]
  next

let kmp p =
  let next = init_next p
  let m = Array.length p
  fun s ->
    let n = Array.length s
    let  i = ref 0
    let j = ref 0 in
    while !j < m && !i < n do
      if s.[!i] = p.[!j] then begin incr i; incr j end else
      if !j = 0 then incr i else j := next.[!j]
    done;
    if !j >= m then Some(!i - m) else None
