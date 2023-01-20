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