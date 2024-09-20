namespace Suave

/// Inspired by https://github.com/NancyFx/Nancy/blob/45860c82e4df8e2d380997ddf1d19d61400fb145/src/Nancy/RequestHeaders.cs
module Headers =
  module Fields =
    module Response =
      [<CompiledName ("SetCookie")>]
      let [<Literal>] setCookie = "Set-Cookie"

  let private toLower (s:string) = s.ToLowerInvariant()

  /// Parse a DateTime as given in the 'Date' Header field.
  let parseDateTime s =
    match System.DateTime.TryParseExact(s, "R", System.Globalization.CultureInfo.InvariantCulture, System.Globalization.DateTimeStyles.None) with
    | true, v -> Some v
    | _ -> None

  /// Parse a decimal as given in a header field.
  let parseDecimal(s: string) =
    match System.Decimal.TryParse(s, System.Globalization.NumberStyles.Number, System.Globalization.CultureInfo.InvariantCulture) with
    | true, d -> Some d
    | _ -> None

  /// Parse a culture info as given in the 'Accept-Language' Header field.
  let parseCultureInfo =
    let cultureNames =
      new System.Collections.Generic.HashSet<string>(
        System.Globalization.CultureInfo.GetCultures(System.Globalization.CultureTypes.AllCultures)
        |> Seq.map (fun c -> c.Name), System.StringComparer.OrdinalIgnoreCase)

    let isValidCulture s =
      System.String.IsNullOrWhiteSpace s |> not &&
      cultureNames.Contains s

    let parseCultureInfo s =
      if isValidCulture s then
        System.Globalization.CultureInfo(s) |> Some
      else None

    parseCultureInfo

  /// Return the first header value with the given name.
  let getFirstHeader name ctx =
    let lowerName = toLower name
    ctx.request.headers
    |> Seq.tryFind (fst >> (=) lowerName)
    |> Option.map snd

  /// Return all headers with the given name.
  let getHeader name ctx =
    let lowerName = toLower name
    ctx.request.headers
    |> Seq.filter (fst >> (=) lowerName)
    |> Seq.map snd

  /// group headers by name and collect all headers in a dictionary with format String<label>:Seq<String<values>>
  let getHeaders ctx =
    ctx.request.headers
    |> Seq.groupBy fst
    |> Seq.map (fun (k,v) -> k, Seq.map snd v)
    |> dict
    |> fun d -> new System.Collections.Generic.Dictionary<_,_>(d, System.StringComparer.OrdinalIgnoreCase)
    :> System.Collections.Generic.IDictionary<_,_>

  /// Split the given header values.
  let getSplitValues headers =
    headers
    |> Seq.collect (fun (x:string) -> x.Split([|','|], System.StringSplitOptions.RemoveEmptyEntries))
    |> Seq.map (fun x -> x.Trim())

  /// order the given split header values by their weight (given via 'q=')
  let getWeightedValues splitValues =
    splitValues
    |> Seq.map (fun (x:string) ->
      let mediaRange, sections =
        match x.Split([|';'|], System.StringSplitOptions.RemoveEmptyEntries)
              |> Seq.map (fun x -> x.Trim())
              |> Seq.toList with
        | mediaRange :: sections -> mediaRange, sections
        | _ -> failwith "expected at least one header!"

      let qVal, others =
        sections
        |> List.partition (fun x -> x.StartsWith("q=", System.StringComparison.OrdinalIgnoreCase))

      let quality =
        match qVal
              |> Seq.map (fun x -> x.Substring 2)
              |> Seq.choose parseDecimal
              |> Seq.tryFind (fun _ -> true) with
        | Some d -> d
        | None -> 1m

      let mediaRange =
        Seq.append [mediaRange] others
        |> String.concat ";"
      mediaRange, quality)
    |> Seq.sortBy (fun (_, q) -> -q)

  /// Headers are lowercased, so can use string.Equals
  open System.Collections.Generic
  let getAll (target : List<string*string>) (key : string) =
    let results = target |> Seq.choose (fun (a, b) -> if a.Equals key then Some b else None)
    if Seq.isEmpty results then
      Choice2Of2 ("Couldn't find key '" + key + "'")
    else
      Choice1Of2 results
