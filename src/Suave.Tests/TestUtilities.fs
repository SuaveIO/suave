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
open Logary.Facade
open Expecto
open FsCheck

type Arbs =
  static member String () =
    Arb.Default.String ()
    |> Arb.filter (fun str -> str <> null)

let fsCheckConfig =
  { FsCheckConfig.defaultConfig with
      maxTest = 100
      arbitrary = [ typeof<Arbs> ] }

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
