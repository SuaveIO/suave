#if INTERACTIVE
#load "Async.fs"
open Suave.Async
#load "Utils.fs"
#load "Globals.fs"
open Suave
#else
module Suave.Log
#endif

open System
open System.Diagnostics

open Suave.Utils.RandomExtensions

/// The log levels specify the severity of the message.
[<CustomEquality; CustomComparison>]
type LogLevel =
  /// The most verbose log level, more verbose than Debug.
  | Verbose
  /// Less verbose than Verbose, more verbose than Info
  | Debug
  /// Less verbose than Debug, more verbose than Warn
  | Info
  /// Less verbose than Info, more verbose than Error
  | Warn
  /// Less verbose than Warn, more verbose than Fatal
  | Error
  /// The least verbose level. Will only pass through fatal
  /// log lines that cause the application to crash or become
  /// unusable.
  | Fatal
  with
    /// Convert the LogLevel to a string
    override x.ToString () =
      match x with
      | Verbose -> "verbose"
      | Debug -> "debug"
      | Info -> "info"
      | Warn -> "warn"
      | Error -> "error"
      | Fatal -> "fatal"

    /// Converts the string passed to a Loglevel.
    [<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
    static member FromString str =
      match str with
      | "verbose" -> Verbose
      | "debug" -> Debug
      | "info" -> Info
      | "warn" -> Warn
      | "error" -> Error
      | "fatal" -> Fatal
      | _ -> Info

    /// Turn the LogLevel into an integer
    [<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
    member x.ToInt () =
      (function
      | Verbose -> 1
      | Debug -> 2
      | Info -> 3
      | Warn -> 4
      | Error -> 5
      | Fatal -> 6) x

    /// Turn an integer into a LogLevel
    [<System.ComponentModel.EditorBrowsable(System.ComponentModel.EditorBrowsableState.Never)>]
    static member FromInt i =
      (function
      | 1 -> Verbose
      | 2 -> Debug
      | 3 -> Info
      | 4 -> Warn
      | 5 -> Error
      | 6 -> Fatal
      | _ as i -> failwith "rank %i not available" i) i

    static member op_LessThan (a, b) = (a :> IComparable<LogLevel>).CompareTo(b) < 0
    static member op_LessThanOrEqual (a, b) = (a :> IComparable<LogLevel>).CompareTo(b) <= 0
    static member op_GreaterThan (a, b) = (a :> IComparable<LogLevel>).CompareTo(b) > 0
    static member op_GreaterThanOrEqual (a, b) = (a :> IComparable<LogLevel>).CompareTo(b) >= 0

    override x.Equals other = (x :> IComparable).CompareTo other = 0

    override x.GetHashCode () = x.ToInt ()

    interface IComparable with
      member x.CompareTo other =
        match other with
        | null -> 1
        | :? LogLevel as tother ->
          (x :> IComparable<LogLevel>).CompareTo tother
        | _ -> failwith <| sprintf "invalid comparison %A to %A" x other

    interface IComparable<LogLevel> with
      member x.CompareTo other =
        compare (x.ToInt()) (other.ToInt())

    interface IEquatable<LogLevel> with
      member x.Equals other =
        x.ToInt() = other.ToInt()

/// A record that keeps track of what request this is.
/// In an uint64 there are 18 446 744 073 709 551 616 number
/// of possible values, so you can be fairly certain a given request
/// id is unique, given a good random number generator.
type TraceHeader =
    /// if this is the 'first' traced request, then trace_id equals
    /// req_id. If it's the second, then trace_id = req_parent_id
    /// or otherwise third or later then trace_id, req_id and req_parent_id
    /// are all disjunct
  { trace_id      : uint64
    /// the request id assigned when suave received the http request
    /// In ZipKin/Dapper-speak, this is the span id
  ; req_id        : uint64
    /// possibly a parent
    /// In ZipKin/Dapper-speak, this is the span parent id
  ; req_parent_id : uint64 option }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module TraceHeader =
  /// The empty trace header has zeroes for trace and request id.
  let empty =
    { trace_id      = 0UL
    ; req_id        = 0UL
    ; req_parent_id = None }

  /// Create a new `TraceHeader` with the given `trace_id` and `span_parent_id`.
  /// This generates a new id and places it in `trace_id` AND `req_id` if no
  /// `trace_id` parameter is supplied. Unless `span_parent_id` is given, that
  /// field is defaulted to None, as suave cannot know the "origin span", so to
  /// speak.
  let mk trace_id span_parent_id =
    let new_id = Globals.random.NextUInt64()
    { trace_id      = trace_id |> Option.or_default new_id
    ; req_id        = new_id
    ; req_parent_id = span_parent_id }

/// When logging, write a log line like this with the source of your
/// log line as well as a message and an optional exception.
type LogLine =
    /// the trace id and span id
    /// If using tracing, then this LogLine is an annotation to a
    /// span instead of a 'pure' log entry
  { trace         : TraceHeader
    /// the level that this log line has
  ; level         : LogLevel
    /// the source of the log line, e.g. 'ModuleName.FunctionName'
  ; path          : string
    /// the message that the application wants to log
  ; message       : string
    /// an optional exception
  ; ``exception`` : exn option
    /// timestamp when this log line was created
  ; ts_utc_ticks  : int64 }

/// The primary Logger abstraction that you can log data into
type Logger =
  /// log - evaluate the function if the log level matches - by making it
  /// a function we don't needlessly need to evaluate it
  /// Calls to this method must be thread-safe and not change any state
  abstract member Log : LogLevel -> (unit -> LogLine) -> unit

module Loggers =
  /// A logger to use for combining a number of other loggers
  type CombiningLogger(other_loggers : Logger list) =
    interface Logger with
      member x.Log level f_line =
        other_loggers |> List.iter (fun l -> l.Log level f_line)

  /// let the ISO8601 love flow
  let internal default_formatter (line : LogLine) =
    // [I] 2014-04-05T12:34:56Z: Hello World! [my.sample.app]
    "[" + Char.ToUpperInvariant(line.level.ToString().[0]).ToString() + "] " +
    (DateTime(line.ts_utc_ticks, DateTimeKind.Utc).ToString("o")) + ": " +
    line.message + " [" + line.path + "]" +
    (match line.``exception`` with | Some e -> " exn:\n" + e.ToString() | None -> "")

  /// Log a line with the given format, printing the current time in UTC ISO-8601 format
  /// and then the string, like such:
  /// '2013-10-13T13:03:50.2950037Z: today is the day'
  type ConsoleWindowLogger(min_level, ?formatter, ?colourise, ?original_color, ?console_semaphore) =
    let sem            = defaultArg console_semaphore (obj())
    let original_color = defaultArg original_color Console.ForegroundColor
    let formatter      = defaultArg formatter default_formatter
    let colourise      = defaultArg colourise true
    let write          = System.Console.WriteLine : string -> unit

    let to_color = function
      | Verbose -> ConsoleColor.DarkGreen
      | Debug -> ConsoleColor.Green
      | Info -> ConsoleColor.White
      | Warn -> ConsoleColor.Yellow
      | Error -> ConsoleColor.DarkRed
      | Fatal -> ConsoleColor.Red

    let log color line =
      if colourise then
        lock sem <| fun _ ->
          Console.ForegroundColor <- color
          (write << formatter) line
          Console.ForegroundColor <- original_color
      else
        // we don't need to take another lock, since Console.WriteLine does that for us
        (write << formatter) line

    interface Logger with
      member x.Log level f = if level >= min_level then log (to_color level) (f ())

  type OutputWindowLogger(min_level, ?formatter) =
    let formatter = defaultArg formatter default_formatter
    let log line = System.Diagnostics.Debug.WriteLine(formatter line)
    interface Logger with
      member x.Log level f_line = if level >= min_level then log (f_line ())

  let sane_defaults_for level =
    if level >= Warn then
      ConsoleWindowLogger(level) :> Logger
    else
      CombiningLogger(
        [ ConsoleWindowLogger(level)
          OutputWindowLogger(level) ]) :> Logger

let internal mk_line path trace ex message =
  { message       = message
  ; level         = Verbose
  ; path          = path
  ; ``exception`` = ex
  ; trace         = trace
  ; ts_utc_ticks  = Globals.utc_now().Ticks }

let verbose (logger : Logger) path trace message =
  logger.Log Verbose (fun _ -> mk_line path trace None message)

let verbosef logger path trace f_format =
  f_format (Printf.kprintf (verbose logger path trace))

let verbosee (logger : Logger) path trace ex message =
  logger.Log Verbose (fun _ -> mk_line path trace (Some ex) message)

let intern (logger : Logger) path =
  verbose logger path (TraceHeader.empty)

let interne (logger : Logger) path =
  verbosee logger path (TraceHeader.empty)

let internf (logger : Logger) path f_format =
  f_format (Printf.kprintf (verbose logger path (TraceHeader.empty)))
