[<AutoOpen>]
module internal Suave.Utils.YoLo

#nowarn "64"

open System
open System.Threading.Tasks

let curry f a b = f (a, b)

let uncurry f (a, b) = f a b

let flip f a b = f b a

module Choice =

  let create v = Choice1Of2 v

  let createSnd v = Choice2Of2 v

  let map f = function
    | Choice1Of2 v -> Choice1Of2 (f v)
    | Choice2Of2 msg -> Choice2Of2 msg

  let mapSnd f = function
    | Choice1Of2 v -> Choice1Of2 v
    | Choice2Of2 v -> Choice2Of2 (f v)

  let bind (f : 'a -> Choice<'b, 'c>) (v : Choice<'a, 'c>) =
    match v with
    | Choice1Of2 v -> f v
    | Choice2Of2 c -> Choice2Of2 c

  let bindSnd (f : 'a -> Choice<'c, 'b>) (v : Choice<'c, 'a>) =
    match v with
    | Choice1Of2 x -> Choice1Of2 x
    | Choice2Of2 x -> f x

  let apply f v =
    bind (fun f' ->
      bind (fun v' ->
        create (f' v')) v) f

  let applySnd f v =
    bind (fun f' ->
      bindSnd (fun v' ->
        createSnd (f' v')) v) f

  let lift2 f v1 v2 =
    apply (apply (create f) v1) v2

  let lift3 f v1 v2 v3 =
    apply (apply (apply (create f) v1) v2) v3

  let lift4 f v1 v2 v3 v4 =
    apply (apply (apply (apply (create f) v1) v2) v3) v4

  let lift5 f v1 v2 v3 v4 v5 =
    apply (apply (apply (apply (apply (create f) v1) v2) v3) v4) v5

  let ofOption onMissing = function
    | Some x -> Choice1Of2 x
    | None   -> Choice2Of2 onMissing

  let inject f = function
    | Choice1Of2 x -> f x; Choice1Of2 x
    | Choice2Of2 x -> Choice1Of2 x

  let injectSnd f = function
    | Choice1Of2 x -> Choice1Of2 x
    | Choice2Of2 x -> f x; Choice1Of2 x

  module Operators =

    let inline (>>=) m f =
      bind f m

    let inline (>>-) m f = // snd
      bindSnd f m

    let inline (=<<) f m =
      bind f m

    let inline (-<<) f m = // snd
      bindSnd f m

    let inline (>>*) m f =
      inject f m

    let inline (>>@) m f = // snd
      injectSnd f m

    let inline (<*>) f m =
      apply f m

    let inline (<!>) f m =
      map f m

    let inline (>!>) m f =
      map f m

    let inline (<@>) f m = // snd
      mapSnd f m

    let inline (>@>) m f = // snd
      mapSnd f m

    let inline ( *>) m1 m2 =
      lift2 (fun _ x -> x) m1 m2

    let inline ( <*) m1 m2 =
      lift2 (fun x _ -> x) m1 m2


module Option =

  let create x = Some x

  let apply (f : ('a -> 'b) option) (v : 'a option) =
    Option.bind (fun f' ->
      Option.bind (fun v' ->
        create (f' v')) v) f

  let lift2 f v1 v2 =
    apply (apply (create f) v1) v2

  let lift3 f v1 v2 v3 =
    apply (apply (apply (create f) v1) v2) v3

  let lift4 f v1 v2 v3 v4 =
    apply (apply (apply (apply (create f) v1) v2) v3) v4

  let lift5 f v1 v2 v3 v4 v5 =
    apply (apply (apply (apply (apply (create f) v1) v2) v3) v4) v5

  let ofChoice = function
    | Choice1Of2 x -> Some x
    | _ -> None

  let toChoice case2 = function
    | Some x -> Choice1Of2 x
    | None   -> Choice2Of2 case2

  let ofNullable nullable : 'a option =
    match box nullable with
    | null -> None // CLR null
    | :? Nullable<_> as n when not n.HasValue -> None // CLR struct
    | :? Nullable<_> as n when n.HasValue -> Some (n.Value) // CLR struct
    | x when x.Equals (DBNull.Value) -> None // useful when reading from the db into F#
    | x -> Some (unbox x) // anything else

  let toNullable = function
    | Some item -> new Nullable<_>(item)
    | None      -> new Nullable<_>()

  let orDefault x = function
    | None -> x
    | Some y -> y

  let inject f = function
    | Some x -> f x; Some x
    | None   -> None

  module Operators =

    let inline (>>=) m f =
      Option.bind f m

    let inline (=<<) f m =
      Option.bind f m

    let inline (>>*) m f =
      inject f m

    let inline (<*>) f m =
      apply f m

    let inline (<!>) f m =
      Option.map f m

    let inline ( *>) m1 m2 =
      lift2 (fun _ x -> x) m1 m2

    let inline ( <*) m1 m2 =
      lift2 (fun x _ -> x) m1 m2

type Base64String = string

module String =
  open System.IO
  open System.Security.Cryptography
  open System.Globalization

  /// Also, invariant culture
  let equals (a : string) (b : string) =
#if DNXCORE50
    (CultureInfo.InvariantCulture.CompareInfo.GetStringComparer(CompareOptions.None)).Equals(a, b)
#else
    a.Equals(b, StringComparison.InvariantCulture)
#endif

  /// Also, invariant culture
  let equalsCaseInsensitve (a : string) (b : string) =
#if DNXCORE50
    (CultureInfo.InvariantCulture.CompareInfo.GetStringComparer(CompareOptions.IgnoreCase)).Equals(a, b)
#else
    a.Equals(b, StringComparison.InvariantCultureIgnoreCase)
#endif
    
  /// Compare ordinally with ignore case.
  let equalsOrdinalCI (str1 : string) (str2 : string) =
    String.Equals(str1, str2, StringComparison.OrdinalIgnoreCase)

  /// Ordinally compare two strings in constant time, bounded by the length of the
  /// longest string.
  let equalsConstantTime (str1 : string) (str2 : string) =
    let mutable xx = uint32 str1.Length ^^^ uint32 str2.Length
    let mutable i = 0
    while i < str1.Length && i < str2.Length do
      xx <- xx ||| uint32 (int str1.[i] ^^^ int str2.[i])
      i <- i + 1
    xx = 0u

  let toLowerInvariant (str : string) =
    str.ToLowerInvariant()

  let replace (find : string) (replacement : string) (str : string) =
    str.Replace(find, replacement)

  let isEmpty (s : string) =
    s.Length = 0

  let trim (s : string) =
    s.Trim()
  
  let trimc (toTrim : char) (s : string) =
    s.Trim toTrim
  
  let trimStart (s : string) =
    s.TrimStart()
  
  let split (c : char) (s : string) =
    s.Split c |> Array.toList
  
  let splita (c : char) (s : string) =
    s.Split c
  
  let startsWith (substring : string) (s : string) =
    s.StartsWith substring
  
  let contains (substring : string) (s : string) =
    s.Contains substring
  
  let substring index (s : string) =
    s.Substring index

module Bytes =
  open System.IO
  open System.Linq
  open System.Security.Cryptography

  let hash (algo : HashAlgorithm) (bs : byte[]) =
    use ms = new MemoryStream()
    ms.Write(bs, 0, bs.Length)
    ms.Seek(0L, SeekOrigin.Begin) |> ignore
    use sha = algo
    sha.ComputeHash ms

  let sha1 =
#if DNXCORE50
    hash (SHA1.Create())
#else
    hash (new SHA1Managed())
#endif

  let sha256 =
#if DNXCORE50
    hash (SHA256.Create())
#else
    hash (new SHA256Managed())
#endif

  let sha512 =
#if DNXCORE50
    hash (SHA512.Create())
#else
    hash (new SHA512Managed())
#endif

  let toHex (bs : byte[]) =
    BitConverter.ToString bs
    |> String.replace "-" ""
    |> String.toLowerInvariant

  let fromHex (digestString : string) =
    Enumerable.Range(0, digestString.Length)
              .Where(fun x -> x % 2 = 0)
              .Select(fun x -> Convert.ToByte(digestString.Substring(x, 2), 16))
              .ToArray()

  /// Compare two byte arrays in constant time, bounded by the length of the
  /// longest byte array.
  let equalsConstantTime (bits : byte []) (bobs : byte []) =
    let mutable xx = uint32 bits.Length ^^^ uint32 bobs.Length
    let mutable i = 0
    while i < bits.Length && i < bobs.Length do
      xx <- xx ||| uint32 (bits.[i] ^^^ bobs.[i])
      i <- i + 1
    xx = 0u

module Map =

  /// put a key to the map; if it's not there already, just add it
  /// otherwise, remove the existing key and place it there.
  let put k v m =
    match m |> Map.tryFind k with
    | Some _ -> m |> Map.remove k |> Map.add k v
    | None -> m |> Map.add k v

[<RequireQualifiedAccess>]
module Culture =
  open System.Globalization

  let invariant = CultureInfo.InvariantCulture

module UTF8 =
  open System.Text

  let private utf8 = Encoding.UTF8

  /// Convert the full buffer `b` filled with UTF8-encoded strings into a CLR
  /// string.
  let toString (bs : byte []) =
    utf8.GetString bs

  /// Convert the byte array to a string, by indexing into the passed buffer `b`
  /// and taking `count` bytes from it.
  let toStringAtOffset (b : byte []) (index : int) (count : int) =
    utf8.GetString(b, index, count)

  /// Get the UTF8-encoding of the string.
  let bytes (s : string) =
    utf8.GetBytes s

  /// Convert the passed string `s` to UTF8 and then encode the buffer with
  /// base64.
  let encodeBase64 : string -> Base64String =
    bytes >> Convert.ToBase64String

  /// Convert the passed string `s`, assumed to be a valid Base64 encoding, to a
  /// CLR string, going through UTF8.
  let decodeBase64 : Base64String -> string =
    Convert.FromBase64String >> toString

  let sha1 =
    bytes >> Bytes.sha1

  let sha1Hex =
    bytes >> Bytes.sha1 >> Bytes.toHex

  let sha256 =
    bytes >> Bytes.sha256

  let sha256Hex =
    bytes >> Bytes.sha256 >> Bytes.toHex

  let sha512 =
    bytes >> Bytes.sha512

  let sha512Hex =
    bytes >> Bytes.sha512 >> Bytes.toHex

module Comparisons =

  /// compare x to yobj mapped on selected value from function f
  let compareOn f x (yobj: obj) =
    match yobj with
    | :? 'T as y -> compare (f x) (f y)
    | _ -> invalidArg "yobj" "cannot compare values of different types"

  /// check equality on x and y mapped on selected value from function f
  let equalsOn f x (yobj:obj) =
    match yobj with
    | :? 'T as y -> (f x = f y)
    | _ -> false

  /// hash x on the selected value from f
  let hashOn f x =  hash (f x)

type Random with
  /// generate a new random ulong64 value
  member x.NextUInt64() =
    let buffer = Array.zeroCreate<byte> sizeof<UInt64>
    x.NextBytes buffer
    BitConverter.ToUInt64(buffer, 0)

module Array =

  /// Ordinally compare two arrays in constant time, bounded by the length of the
  /// longest array. This function uses the F# language equality.
  let equalsConstantTime (arr1 : 'a []) (arr2 : 'a []) =
    if arr1.Length <> arr2.Length then false else
    let mutable b = true
    for i in 0 .. arr1.Length - 1 do
      b <- b && (arr1.[i] = arr2.[i])
    b

module Regex =
  open System.Text.RegularExpressions

  let escape input =
    Regex.Escape input

  let split pattern input =
    Regex.Split(input, pattern)
    |> List.ofArray

  let replace pattern replacement input =
    Regex.Replace(input, pattern, (replacement : string))

  let ``match`` pattern input =
    match Regex.Matches(input, pattern) with
    | x when x.Count > 0 ->
      x
      |> Seq.cast<Match>
      |> Seq.head
      |> fun x -> x.Groups
      |> Some
    | _ -> None

type Microsoft.FSharp.Control.Async with
  /// Raise an exception on the async computation/workflow.
  static member AsyncRaise (e : exn) =
    Async.FromContinuations(fun (_,econt,_) -> econt e)

  /// Await a task asynchronously
  static member AwaitTask (t : Task) =
    let flattenExns (e : AggregateException) = e.Flatten().InnerExceptions.[0]
    let rewrapAsyncExn (it : Async<unit>) =
      async { try do! it with :? AggregateException as ae -> do! Async.AsyncRaise (flattenExns ae) }
    let tcs = new TaskCompletionSource<unit>(TaskCreationOptions.None)
    t.ContinueWith((fun t' ->
      if t.IsFaulted then tcs.SetException(t.Exception |> flattenExns)
      elif t.IsCanceled then tcs.SetCanceled ()
      else tcs.SetResult(())), TaskContinuationOptions.ExecuteSynchronously)
    |> ignore
    tcs.Task |> Async.AwaitTask |> rewrapAsyncExn

type Microsoft.FSharp.Control.AsyncBuilder with
  /// An extension method that overloads the standard 'Bind' of the 'async' builder. The new overload awaits on
  /// a standard .NET task
  member x.Bind(t : Task<'T>, f:'T -> Async<'R>) : Async<'R> =
    async.Bind(Async.AwaitTask t, f)

  /// An extension method that overloads the standard 'Bind' of the 'async' builder. The new overload awaits on
  /// a standard .NET task which does not commpute a value
  member x.Bind(t : Task, f : unit -> Async<'R>) : Async<'R> =
    async.Bind(Async.AwaitTask t, f)

module Async =

  let result = async.Return

  let map f value = async {
    let! v = value
    return f v
  }

  let bind f xAsync = async {
    let! x = xAsync
    return! f x
  }

  let apply fAsync xAsync = async {
    // start the two asyncs in parallel
    let! fChild = Async.StartChild fAsync
    let! xChild = Async.StartChild xAsync

    // wait for the results
    let! f = fChild
    let! x = xChild

    // apply the function to the results
    return f x
  }

  let lift2 f x y =
    apply (apply (result f) x) y

  let lift3 f x y z =
    apply (apply (apply (result f) x) y) z

  let lift4 f x y z a =
    apply (apply (apply (apply (result f) x) y) z) a

  let lift5 f x y z a b =
    apply (apply (apply (apply (apply (result f) x) y) z) a) b

  module Operators =

    let inline (>>=) m f =
      bind f m

    let inline (=<<) f m =
      bind f m

    let inline (<*>) f m =
      apply f m

    let inline (<!>) f m =
      map f m

    let inline ( *>) m1 m2 =
      lift2 (fun _ x -> x) m1 m2

    let inline ( <*) m1 m2 =
      lift2 (fun x _ -> x) m1 m2

module List =

  /// Split xs at n, into two lists, or where xs ends if xs.Length < n.
  let split n xs =
    let rec splitUtil n xs acc =
      match xs with
      | [] -> List.rev acc, []
      | _ when n = 0u -> List.rev acc, xs
      | x::xs' -> splitUtil (n - 1u) xs' (x::acc)
    splitUtil n xs []

  /// Chunk a list into pageSize large chunks
  let chunk pageSize = function
    | [] -> None
    | l -> let h, t = l |> split pageSize in Some(h, t)

  let first = function
    | [] -> None
    | x :: _ -> Some x

  // Description of the below functions:
  // http://fsharpforfunandprofit.com/posts/elevated-world-5/#asynclist

  /// Map a Async producing function over a list to get a new Async using
  /// applicative style. ('a -> Async<'b>) -> 'a list -> Async<'b list>
  let rec traverseAsyncA f list =
    let (<*>) = Async.apply
    let cons head tail = head :: tail
    let initState = Async.result []
    let folder head tail =
      Async.result cons <*> (f head) <*> tail

    List.foldBack folder list initState

  /// Transform a "list<Async>" into a "Async<list>" and collect the results
  /// using apply.
  let sequenceAsyncA x = traverseAsyncA id x

  /// Map a Choice-producing function over a list to get a new Choice using
  /// applicative style. ('a -> Choice<'b, 'c>) -> 'a list -> Choice<'b list, 'c>
  let rec traverseChoiceA f list =
    let (<*>) = Choice.apply
    let cons head tail = head :: tail

    // right fold over the list
    let initState = Choice.create []
    let folder head tail =
      Choice.create cons <*> (f head) <*> tail

    List.foldBack folder list initState

  /// Transform a "list<Choice>" into a "Choice<list>" and collect the results
  /// using apply.
  let sequenceChoiceA x = traverseChoiceA id x

module Seq =

  let combinations size set =
    let rec combinations' acc size set =
      seq {
        match size, set with
        | n, x::xs ->
            if n > 0 then yield! combinations' (x::acc) (n - 1) xs
            if n >= 0 then yield! combinations' acc n xs
        | 0, [] -> yield acc
        | _, [] -> ()
      }
    combinations' [] size set

  let first (xs : _ seq) : _ option =
    if Seq.isEmpty xs then None else Seq.head xs |> Some

module Env =

  let var (k : string) =
    match Environment.GetEnvironmentVariable k with
    | null -> None
    | v    -> Some v

  let varDefault (key : String) (def : string) =
    match var key with
    | Some v -> v
    | None -> def

  let varRequired (k : String) =
    match var k with
    | Some v -> v
    | None ->failwithf "missing environment var %s" k

module App =

  open System.IO
  open System.Reflection

  /// Gets the calling assembly's informational version number as a string
  let getVersion () =
#if DNXCORE50
    (typeof<Random>.GetTypeInfo().Assembly)
#else
    Assembly.GetCallingAssembly()
#endif
            .GetCustomAttribute<AssemblyInformationalVersionAttribute>()
            .InformationalVersion

  /// Get the assembly resource
  let resource name =
#if DNXCORE50
    let assembly = typeof<Random>.GetTypeInfo().Assembly
#else
    let assembly = Assembly.GetExecutingAssembly ()
#endif
    use stream = assembly.GetManifestResourceStream name
    if stream = null then
      assembly.GetManifestResourceNames()
      |> Array.fold (fun s t -> sprintf "%s\n - %s" s t) ""
      |> sprintf "couldn't find resource named '%s', from: %s" name
      |> Choice2Of2
    else
      use reader = new StreamReader(stream)
      reader.ReadToEnd ()
      |> Choice1Of2
