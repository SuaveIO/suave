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
open Suave.Web
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

type Assert with
  static member StreamsEqual(msg, s1 : Stream, s2 : Stream) =
    let buf = Array.zeroCreate<byte> 2
    let rec compare pos =
      match s1.Read(buf, 0, 1), s2.Read(buf, 1, 1) with
      | x, y when x <> y -> Tests.failtestf "Not equal at pos %d" pos
      | 0, _ -> ()
      | _ when buf.[0] <> buf.[1] -> Tests.failtestf "Not equal at pos %d" pos
      | _ -> compare (pos + 1)
    compare 0

/// Entry for the inspectable log
type InspectableLogEntry =
  {
    /// The level of the entry logged
    level : LogLevel
    /// The function that provided the value for the log entry
    value : unit -> LogLine
  }

/// A logger that can be inspected to see what was logged
type InspectableLog() =
  member val logs : InspectableLogEntry list = [] with get, set

  interface Logger with
    member x.Log level lineFunc =
      x.logs <- { level = level; value = lineFunc } :: x.logs