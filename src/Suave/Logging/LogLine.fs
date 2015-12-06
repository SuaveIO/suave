namespace Suave.Logging

open Suave

/// When logging, write a log line like this with the source of your
/// log line as well as a message and an optional exception.
type LogLine =
  { /// the trace id and span id
    /// If using tracing, then this LogLine is an annotation to a
    /// span instead of a 'pure' log entry
    trace         : TraceHeader
    /// the level that this log line has
    level         : LogLevel
    /// the source of the log line, e.g. 'ModuleName.FunctionName'
    path          : string
    /// the message that the application wants to log
    message       : string
    /// an optional exception
    ``exception`` : exn option
    /// timestamp when this log line was created
    tsUTCTicks    : int64 }

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogLine =
  let mk path level trace ex message =
    { message       = message
      level         = level
      path          = path
      ``exception`` = ex
      trace         = trace
      tsUTCTicks    = Globals.utcNow().Ticks }

  let trace_ =
    (fun x -> x.trace),
    fun v (x : LogLine) -> { x with trace = v }

  let level_ =
    (fun x -> x.level),
    fun v (x : LogLine) -> { x with level = v }

  let path_ =
    (fun x -> x.path),
    fun v (x : LogLine) -> { x with path = v }

  let message_ =
    (fun x -> x.message),
    fun v (x : LogLine) -> { x with message = v }

  let tsUTCTicks_ =
    (fun x -> x.tsUTCTicks),
    fun v (x : LogLine) -> { x with tsUTCTicks = v }

  let exception_ =
    (fun x -> x.``exception``),
    fun v x -> { x with ``exception`` = v }