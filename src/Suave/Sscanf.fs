module Suave.Sscanf

open System
open System.Diagnostics.CodeAnalysis
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

[<Literal>]
let private DynamicCodeMessage =
  "Suave.Sscanf reconstructs tuples via FSharp.Reflection (FSharpType.GetTupleElements / FSharpValue.MakeTuple). \
This requires runtime code generation and is not compatible with .NET Native AOT. \
Use Suave.Router for AOT scenarios."

/// Verify that f x, and then return x, otherwise fail witha 'format failure' message
let private check f x = if f x then x else failwith $"format failure \"{x}\""

let private parseDecimal (x: string) = Decimal.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

/// The supported characters for the formatter
let parsers =
  dict [
    'b', (fun (s:string) -> Boolean.Parse s) >> box
    'd', int64 >> box
    'i', int64 >> box
    's', box
    'u', uint64 >> box
    'x', check (String.forall Char.IsLower) >> ((+) "0x") >> int64 >> box
    'X', check (String.forall Char.IsUpper) >> ((+) "0x") >> int64 >> box
    'o', ((+) "0o") >> int64 >> box
    'e', float >> box // no check for correct format for floats
    'E', float >> box
    'f', float >> box
    'F', float >> box
    'g', float >> box
    'G', float >> box
    'M', parseDecimal >> box
    'c', char >> box
  ]

// array of all possible formatters, i.e. [|"%b"; "%d"; ...|]
let separators =
  parsers.Keys
  |> Seq.map (fun c -> "%" + c.ToString())
  |> Seq.toArray

// Creates a list of formatter characters from a format string,
// for example "(%s,%d)" -> ['s', 'd']
let rec getFormatters xs =
  match xs with
  | '%' :: '%' :: xr -> getFormatters xr
  | '%' :: x :: xr   ->
    if parsers.ContainsKey x then x :: getFormatters xr
    else failwith $"Unknown formatter %%{x}"
  | x :: xr          -> getFormatters xr
  | []               -> []

// Coerce integer types
let coerce o (v: Type) =
  //printfn "Convert %A to %s" o (v.FullName)
  match v with
  | v when v = typeof<int16> ->
    int16 (unbox<int64> o) |> box
  | v when v = typeof<int32> ->
    int32 (unbox<int64> o) |> box
  | v when v = typeof<int64> ->
    o
  | v when v = typeof<uint16> ->
    uint16 (unbox<uint64> o) |> box
  | v when v = typeof<uint32> ->
    uint32 (unbox<uint64> o) |> box
  | v when v = typeof<uint64> ->
    o
  | _ ->
    o

/// Parse the format in 'pf' from the string 's', failing and raising an exception
/// otherwise
[<RequiresDynamicCode(DynamicCodeMessage)>]
let sscanf (pf:PrintfFormat<_,_,_,_,'t>) s : 't =
  let formatStr  = pf.Value
  let constants  = formatStr.Split([|"%%"|], StringSplitOptions.None)
                   |> Array.map (fun x -> x.Split(separators, StringSplitOptions.None))
  let regexStr   = constants
                   |> Array.map (fun c -> c |> Array.map Regex.Escape |> String.concat "(.*?)")
                   |> String.concat "%"
  let regex      = Regex("^" + regexStr + "$")
  let formatters = formatStr.ToCharArray() // need original string here (possibly with "%%"s)
                   |> Array.toList |> getFormatters
  let groups =
    regex.Match(s).Groups
    |> Seq.cast<Group>
    |> Seq.skip 1

  let matches =
    (groups, formatters)
    ||> Seq.map2 (fun g f -> g.Value |> parsers.[f])
    |> Seq.toArray

  if matches.Length = 1 then
    coerce matches.[0] typeof<'t> :?> 't
  else
    let tupleTypes = FSharpType.GetTupleElements(typeof<'t>)
    let matches =
      (matches,tupleTypes)
      ||> Array.map2 ( fun a b -> coerce a b)
    FSharpValue.MakeTuple(matches, typeof<'t>) :?> 't


/// Parse the format in 'pf' from the string 's' regardless of casing, failing and raising an exception
/// otherwise
[<RequiresDynamicCode(DynamicCodeMessage)>]
let sscanfci (pf:PrintfFormat<_,_,_,_,'t>) s : 't =
  let formatStr  = pf.Value
  let constants  = formatStr.Split([|"%%"|], StringSplitOptions.None)
                   |> Array.map (fun x -> x.Split(separators, StringSplitOptions.None))
  let regexStr   = constants
                   |> Array.map (fun c -> c |> Array.map Regex.Escape |> String.concat "(.*?)")
                   |> String.concat "%"
  let regex      = Regex("^" + regexStr + "$", RegexOptions.IgnoreCase)
  let formatters = formatStr.ToCharArray() // need original string here (possibly with "%%"s)
                   |> Array.toList |> getFormatters
  let groups =
    regex.Match(s).Groups
    |> Seq.cast<Group>
    |> Seq.skip 1

  let matches =
    (groups, formatters)
    ||> Seq.map2 (fun g f -> g.Value |> parsers.[f])
    |> Seq.toArray

  if matches.Length = 1 then
    coerce matches.[0] typeof<'t> :?> 't
  else
    let tupleTypes = FSharpType.GetTupleElements(typeof<'t>)
    let matches =
      (matches,tupleTypes)
      ||> Array.map2 ( fun a b -> coerce a b)
    FSharpValue.MakeTuple(matches, typeof<'t>) :?> 't
