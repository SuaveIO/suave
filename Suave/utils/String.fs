namespace Suave.Utils

module String =
  open System

  /// Ordinally compare two strings in constant time, bounded by the length of the
  /// longest string.
  let eq_ord_cnst_time (str1 : string) (str2 : string) =
    let mutable xx = uint32 str1.Length ^^^ uint32 str2.Length
    let mutable i = 0
    while i < str1.Length && i < str2.Length do
      xx <- xx ||| uint32 (int str1.[i] ^^^ int str2.[i])
      i <- i + 1
    xx = 0u

  /// Compare ordinally with ignore case.
  let eq_ord_ci (str1 : string) (str2 : string) =
    String.Equals(str1, str2, StringComparison.OrdinalIgnoreCase)

  let trim (s : string) =
    s.Trim()

  let split (c : char) (s : string) =
    s.Split c |> Array.toList