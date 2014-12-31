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
    ts_utc_ticks  : int64 }

    
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogLine =
  let mk path level trace ex message =
    { message       = message
      level         = level
      path          = path
      ``exception`` = ex
      trace         = trace
      ts_utc_ticks  = Globals.utc_now().Ticks }