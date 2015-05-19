﻿module Suave.Sscanf

open System
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

/// Verify that f x, and then return x, otherwise fail witha 'format failure' message
let private check f x = if f x then x else failwithf "format failure \"%s\"" x

let private parseDecimal x = Decimal.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

/// The supported characters for the formatter
let parsers =
  dict [
    'b', Boolean.Parse >> box
    'd', int64 >> box
    'i', int64 >> box
    's', box
    'u', uint32 >> int64 >> box
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
    else failwithf "Unknown formatter %%%c" x
  | x :: xr          -> getFormatters xr
  | []               -> []

// Coerce integer types from int64
let coerce o = function
  | v when v = typeof<int32> ->
    int32(unbox<int64> o) |> box
  | v when v = typeof<uint32> ->
    uint32(unbox<int64> o) |> box
  | _ -> o

/// Parse the format in 'pf' from the string 's', failing and raising an exception
/// otherwise
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

module private BasicTesting =
  // some basic testing
  let (a,b)           = sscanf "(%%%s,%M)" "(%hello, 4.53)"
  let aaa : int32     = sscanf "aaaa%d" "aaaa4"
  let bbb : int64     = sscanf "aaaa%d" "aaaa4"
  let (x,y,z)         = sscanf "%s-%s-%s" "test-this-string"
  let (c,d,e : uint32,f,g,h,i) = sscanf "%b-%d-%i,%u,%x,%X,%o" "false-42--31,13,ff,FF,42"
  let (j,k,l,m,n,o,p) = sscanf "%f %F %g %G %e %E %c" "1 2.1 3.4 .3 43.2e32 0 f"

  let aa              = sscanf "(%s)" "(45.33)"
