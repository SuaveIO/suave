module Suave.Tests.TestUtilities

#nowarn "25"

open System
open System.IO
open System.Threading
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Reflection
open System.Text
open Suave
open Suave.Web
open Suave.Logging
open Expecto
open FsCheck

type UTF8Char = UTF8Char of ch:byte[]
type UTF8String =
  UTF8String of str:(string * byte[])[] * characters:uint16 * bytesCount:uint32
with
  /// Returns a .Net string (UTF16) from the chars. Note that the char count in UTF8 doesn't equal
  /// that of UTF16.
  member x.concat () =
    let (UTF8String (chars, ccount, bcount)) = x
    chars |> Array.map fst |> String.Concat

type UTF8LongString = UTF8LongString of string:UTF8String

module Generators =
  let utf8char: Gen<UTF8Char> =
    let intToBytes (i: int) =
      let bs = BitConverter.GetBytes i
      if BitConverter.IsLittleEndian then Array.Reverse bs
      bs

    gen {
      // TODO: consider enabling 4-byte chars after more investigation
      let! charSize = Gen.choose (1, 4)
      match charSize with
      | 1 ->
        let! value = Gen.choose (0x00, 0x7F)
        let bs = intToBytes value
        if bs.Length <> 4 then failwith "len(bs) should be 4"
        return Array.singleton bs.[3]

      | 2 ->
        let! codepoint = Gen.choose (0x0080, 0x07ff)
        let bs = intToBytes codepoint
        return [|
          // MSB byte 1: 110xxxxx
          0b1100_0000uy ||| ((0b0000_0111uy &&& bs.[2]) <<< 2) ||| ((0b1100_0000uy &&& bs.[3]) >>> 6)
          // LSB byte 2: 10xxxxxx
          0b1000_0000uy ||| (0b0011_1111uy &&& bs.[3])
        |]

      | 3 ->
        // Don't generate surrogate characters (U+D800 - U+DFFF), which are invalid on their own.
        // Also don't generate 0xFFFE or 0xFFFF, which are illegal Unicode characters
        let! codepoint = Gen.frequency [ 0xD800 - 0x0800, Gen.choose (0x0800, 0xD7FF)
                                         0xFFFE - 0xE000, Gen.choose (0xE000, 0xFFFD) ]
        let bs = intToBytes codepoint
        return [|
          // byte 1: 1110 xxxx
          0b1110_0000uy ||| ((0b1111_0000uy &&& bs.[2]) >>> 4)
          // byte 2: 10 xxxx xx
          0b1000_0000uy ||| (((0b0000_1111uy &&& bs.[2]) <<< 2) ||| ((0b1100_0000uy &&& bs.[3]) >>> 6))
          // byte 3: 10 xx xxxx
          0b1000_0000uy ||| (0b0011_1111uy &&& bs.[3])
        |]

      | 4 ->
        // https://en.wikipedia.org/wiki/Plane_(Unicode)#Overview
        // Let's skip Supplementary Private Use Area planes
        // let! codepoint = Gen.choose (0x10000, 0x10FFFF)
        let! codepoint = Gen.choose (0x10000, 0x2FFFF)
        let bs = intToBytes codepoint
        return [|
          // byte 1: 11110xxx
          0b1111_0000uy ||| ((0b0001_1100uy &&& bs.[1]) >>> 2)
          // byte 2: 10xxxxxx
          0b1000_0000uy ||| ((0b1111_0000uy &&& bs.[2]) >>> 4) ||| ((0b0000_0011uy &&& bs.[1]) <<< 4)
          // byte 3: 10xxxxxx
          0b1000_0000uy ||| ((0b0000_1111uy &&& bs.[2]) <<< 2) ||| ((0b1100_0000uy &&& bs.[3]) >>> 6)
          // byte 4: 10xxxxxx
          0b1000_0000uy ||| (0b0011_1111uy &&& bs.[3])
        |]

      | _ ->
        return failwith "outside gen"
    }
    |> Gen.map UTF8Char

  let utf8String =
    let baseNoChars = 389
    let utf8 = Encoding.UTF8
    let generator =
      gen {
        let! multiple = Gen.choose (1, 10)
        let! size = Gen.choose (baseNoChars, baseNoChars * multiple)
        // up to 15.2 KiB of data (if all chars are using 4 byte chars)
        let! chars = Gen.arrayOfLength size utf8char
        let combinedString = chars |> Array.map (fun (UTF8Char c) -> utf8.GetString c, c)
        let bytesCount = chars |> Array.sumBy (fun (UTF8Char c) -> c.Length) |> uint32
        return UTF8String (combinedString, uint16 size, bytesCount)
      }
    let shrinker (UTF8String (chars, charCount, bytesCount)) =
      if chars.Length <= 1 then Seq.empty else
      let removedBytes = let _, bs = chars.[0] in bs.Length
      UTF8String (chars.[1..], charCount - 1us, bytesCount - uint32 removedBytes)
      |> Seq.singleton
    generator, shrinker
    // generator

type UTF8Strings() =
  static member UTF8Char(): Arbitrary<UTF8Char> =
    Arb.fromGen Generators.utf8char
  static member UTF8String(): Arbitrary<UTF8String> =
    Arb.fromGenShrink Generators.utf8String
    // Arb.fromGen Generators.utf8String

type Arbs =
  static member String () =
    Arb.Default.String ()
    |> Arb.filter (fun str -> str <> null)

let fsCheckConfig =
  { FsCheckConfig.defaultConfig with
      startSize = 1
      maxTest = 200
      arbitrary =
        [ typeof<Arbs>
          typeof<UTF8Strings>
        ] }

let currentPath =
  Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

let readText relativePath =
  File.ReadAllText(Path.Combine(currentPath, relativePath))

let readBytes relativePath =
  File.ReadAllBytes(Path.Combine(currentPath, relativePath))

let openRead relativePath =
  File.OpenRead (Path.Combine(currentPath, relativePath))

module Expect =
  /// Expect the streams to byte-wise equal.
  let streamsEqual (s1 : IO.Stream) (s2 : IO.Stream) format =
      let buf = Array.zeroCreate<byte> 2
      let rec compare pos =
        match s1.Read(buf, 0, 1), s2.Read(buf, 1, 1) with
        | x, y when x <> y ->
          Tests.failtestf "%s. Not equal at pos %d" format pos
        | 0, _ ->
          ()
        | _ when buf.[0] <> buf.[1] ->
          Tests.failtestf "%s. Not equal at pos %d" format pos
        | _ ->
          compare (pos + 1)
      compare 0

module Assert =
  let Equal(msg, exp, act) = Expect.equal act exp msg

type LogMethod =
  | Factory of (LogLevel -> Message)
  | Plain of Message

/// Entry for the inspectable log
type InspectableLogEntry =
  { /// The level of the entry logged
    level : LogLevel
    /// The function that provided the value for the log entry
    value : LogMethod }

/// A logger that can be inspected to see what was logged
type InspectableLog() =
  member val logs : InspectableLogEntry list = [] with get, set

  interface Logger with

    member x.name = [| "Suave"; "Tests"; "Inspectable" |]

    member x.log level msgFactory =
      x.logs <- { level = level; value = Factory msgFactory } :: x.logs
      async.Return ()

    member x.logWithAck level msgFactory : Async<unit> =
      x.logs <- { level = level; value = Factory msgFactory } :: x.logs
      async.Return ()
