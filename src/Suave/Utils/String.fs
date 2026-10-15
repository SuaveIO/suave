module String

  open System

  /// Also, invariant culture
  let equals (a: string) (b: string) =
    a.Equals(b, StringComparison.InvariantCulture)

  /// Also, invariant culture
  let equalsCaseInsensitive (a: string) (b: string) =
    a.Equals(b, StringComparison.InvariantCultureIgnoreCase)

  /// Compare ordinally with ignore case.
  let equalsOrdinalCI (str1: string) (str2: string) =
    String.Equals(str1, str2, StringComparison.OrdinalIgnoreCase)

  /// True if the comma-separated token list `list` (e.g. a Connection header
  /// value, RFC 9110 §5.6.1) contains `token`, ignoring case and the optional
  /// whitespace around each element. Does not allocate.
  let containsTokenOrdinalCI (token: string) (list: string) =
    let mutable found = false
    let mutable start = 0
    while not found && start <= list.Length do
      let comma = list.IndexOf(',', start)
      let stop = if comma < 0 then list.Length else comma
      let mutable a = start
      let mutable b = stop
      while a < b && (list.[a] = ' ' || list.[a] = '\t') do a <- a + 1
      while b > a && (list.[b - 1] = ' ' || list.[b - 1] = '\t') do b <- b - 1
      if b - a = token.Length
         && String.Compare(list, a, token, 0, token.Length, StringComparison.OrdinalIgnoreCase) = 0 then
        found <- true
      start <- stop + 1
    found

  /// Ordinally compare two strings in constant time, bounded by the length of the
  /// longest string.
  let equalsConstantTime (str1: string) (str2: string) =
    let mutable xx = uint32 str1.Length ^^^ uint32 str2.Length
    let mutable i = 0
    while i < str1.Length && i < str2.Length do
      xx <- xx ||| uint32 (int str1.[i] ^^^ int str2.[i])
      i <- i + 1
    xx = 0u

  let toLowerInvariant (str: string) =
    str.ToLowerInvariant()

  let replace (find: string) (replacement: string) (str: string) =
    str.Replace(find, replacement)

  let isEmpty (s: string) =
    s.Length = 0

  let trim (s: string) =
    s.Trim()

  let trimc (toTrim: char) (s: string) =
    s.Trim toTrim

  let trimStart (s: string) =
    s.TrimStart()

  let split (c: char) (s: string) =
    s.Split c |> Array.toList

  let splita (c: char) (s: string) =
    s.Split c

  let startsWith (substring: string) (s: string) =
    s.StartsWith substring

  let contains (substring: string) (s: string) =
    s.Contains substring

  let substring index (s: string) =
    s.Substring index