module Suave.Sscanf

open System
open System.Text
open System.Text.RegularExpressions
open Microsoft.FSharp.Reflection

let check f x = if f x then x else failwithf "format failure \"%s\"" x

let parseDecimal x = Decimal.Parse(x, System.Globalization.CultureInfo.InvariantCulture)

let parsers = dict [
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
                 'M', parseDecimal >> box
                 'c', char >> box
                ]
// array of all possible formatters, i.e. [|"%b"; "%d"; ...|]
let separators =
   parsers.Keys
   |> Seq.map (fun c -> sprintf "%%%c" c)
   |> Seq.toArray

// Creates a list of formatter characters from a format string,
// for example "(%s,%d)" -> ['s', 'd']
let rec getFormatters xs =
   match xs with
   | '%'::'%'::xr -> getFormatters xr
   | '%'::x::xr -> if parsers.ContainsKey x then x::getFormatters xr
                   else failwithf "Unknown formatter %%%c" x
   | x::xr -> getFormatters xr
   | [] -> []


let sscanf (pf:PrintfFormat<_,_,_,_,'t>) s : 't =
  let formatStr = pf.Value.Replace("%%", "%")
  let constants = formatStr.Split(separators, StringSplitOptions.None)
  let regex = Regex("^" + String.Join("(.*?)", constants |> Array.map Regex.Escape) + "$")
  let formatters = pf.Value.ToCharArray() // need original string here (possibly with "%%"s)
                   |> Array.toList |> getFormatters 
  let groups = 
    regex.Match(s).Groups 
    |> Seq.cast<Group> 
    |> Seq.skip 1

  let matches =
    (groups, formatters)
    ||> Seq.map2 (fun g f -> g.Value |> parsers.[f])
    |> Seq.toArray

  //FSharpValue.MakeTuple(matches, typeof<'t>) :?> 't
  if matches.Length = 1 then matches.[0] :?> 't; else FSharpValue.MakeTuple(matches, typeof<'t>) :?> 't

// some basic testing
let (a,b) = sscanf "(%%%s,%M)" "(%hello, 4.53)"
let (x,y,z) = sscanf "%s-%s-%s" "test-this-string"
let (c,d,e,f,g,h,i) = sscanf "%b-%d-%i,%u,%x,%X,%o" "false-42--31,13,ff,FF,42"
let (j,k,l,m,n,o,p) = sscanf "%f %F %g %G %e %E %c" "1 2.1 3.4 .3 43.2e32 0 f"

let aa = sscanf "(%s)" "(45.33)" //fails
