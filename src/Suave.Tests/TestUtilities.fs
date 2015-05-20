module Suave.Tests.TestUtilities

#nowarn "25"

open System
open System.IO
open System.Threading
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Reflection

open Suave
open Suave.Types
open Suave.Web
open Suave.Http
open Suave.Logging

open FsCheck

open Fuchu

type Arbs =
  static member String () = Arb.Default.String () |> Arb.filter (fun str -> str <> null)

let fsCheckConfig = { Config.Default with Arbitrary = [ typeof<Arbs> ] }

let currentPath =
  Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

let readText relativePath =
  File.ReadAllText(Path.Combine(currentPath, relativePath))

let readBytes relativePath =
  File.ReadAllBytes(Path.Combine(currentPath, relativePath))

let openRead relativePath =
  File.OpenRead (Path.Combine(currentPath, relativePath))

let defaultConfig =
  { defaultConfig with
      logger = Loggers.saneDefaultsFor LogLevel.Warn }

type Assert with
  static member StreamsEqual(msg, s1 : Stream, s2 : Stream) =
    let bufLen = 0x10
    let s1buf, s2buf = Array.zeroCreate<byte> bufLen, Array.zeroCreate<byte> bufLen
    let mutable read = 0
    let mutable pos = 0
    let mutable pos2 = 0
    let mutable eq = true
    let mutable first = true
    while read > 0 && eq || first do
      first <- false
      read <- s1.Read(s1buf, pos, bufLen)
      let s2read = s2.Read(s2buf, pos, bufLen)
      eq <- eq && read = s2read
      for i in 0..read do
        if eq then pos2 <- i // omg what a hack
        eq <- eq && s1buf.[i] = s2buf.[i]
      if eq then
        pos <- pos + read
        pos2 <- 0
    if not eq then Tests.failtestf "The streams to not equal at index %d" (pos + pos2)
