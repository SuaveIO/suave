module Suave.Sscanf

open System
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

/// Verify that f x, and then return x, otherwise fail witha 'format failure' message
let private check f x = if f x then x else failwithf "format failure \"%s\"" x

let private parse_decimal x = Decimal.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

/// The supported characters for the formatter
let parsers =
  dict [
    'b', Boolean.Parse >> box
    'd', int >> box
    'i', int >> box
    's', box
    'u', uint32 >> int >> box
    'x', check (String.forall Char.IsLower) >> ((+) "0x") >> int >> box
    'X', check (String.forall Char.IsUpper) >> ((+) "0x") >> int >> box
    'o', ((+) "0o") >> int >> box
    'e', float >> box // no check for correct format for floats
    'E', float >> box
    'f', float >> box
    'F', float >> box
    'g', float >> box
    'G', float >> box
    'M', parse_decimal >> box
    'c', char >> box
  ]

// array of all possible formatters, i.e. [|"%b"; "%d"; ...|]
let separators =
  parsers.Keys
  |> Seq.map (fun c -> "%" + c.ToString())
  |> Seq.toArray

// Creates a list of formatter characters from a format string,
// for example "(%s,%d)" -> ['s', 'd']
let rec get_formatters xs =
  match xs with
  | '%' :: '%' :: xr -> get_formatters xr
  | '%' :: x :: xr   ->
    if parsers.ContainsKey x then x :: get_formatters xr
    else failwithf "Unknown formatter %%%c" x
  | x :: xr          -> get_formatters xr
  | []               -> []

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
                   |> Array.toList |> get_formatters
  let groups =
    regex.Match(s).Groups
    |> Seq.cast<Group>
    |> Seq.skip 1

  let matches =
    (groups, formatters)
    ||> Seq.map2 (fun g f -> g.Value |> parsers.[f])
    |> Seq.toArray

  if matches.Length = 1 then matches.[0] :?> 't else FSharpValue.MakeTuple(matches, typeof<'t>) :?> 't

module private BasicTesting =
  // some basic testing
  let (a,b)           = sscanf "(%%%s,%M)" "(%hello, 4.53)"
  let (x,y,z)         = sscanf "%s-%s-%s" "test-this-string"
  let (c,d,e,f,g,h,i) = sscanf "%b-%d-%i,%u,%x,%X,%o" "false-42--31,13,ff,FF,42"
  let (j,k,l,m,n,o,p) = sscanf "%f %F %g %G %e %E %c" "1 2.1 3.4 .3 43.2e32 0 f"

  let aa              = sscanf "(%s)" "(45.33)" //fails
