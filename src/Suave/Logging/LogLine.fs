namespace Suave.Logging

open Suave

type LogTemplateFormatter = string -> (string -> obj) -> string

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
    /// the template message that the application wants to log
    template      : string
    /// a function which can format the log template message. When the
    /// message is pre-formatted, this is None. When the log message 
    /// contains structured data, this function can be used to render
    /// the message template with the data.
    formatter     : LogTemplateFormatter option
    /// any key-value based data to log
    data          : Map<string, obj>
    /// an optional exception
    ``exception`` : exn option
    /// timestamp when this log line was created
    tsUTCTicks    : int64 }

    /// Formats the log line as a string
    member x.formatMessage () =
        match x.formatter with
        | None -> x.template
        | Some fmter -> fmter x.template x.GetDataByName

    /// Gets a data value
    member x.GetDataByName name = Option.toObj (x.data.TryFind name)

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module LogLine =
  let mk path level trace ex message =
    { template      = message
      formatter     = None
      data          = Map.empty
      level         = level
      path          = path
      ``exception`` = ex
      trace         = trace
      tsUTCTicks  = Globals.utcNow().Ticks }

  /// assists with type inference, avoiding ugly 'box' calls for each
  /// value item when constructing the log data map 
  let mapLogData (data: (string*obj) list) = Map.ofList data

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
    (fun (x : LogLine) -> x.formatMessage()),
    fun v (x : LogLine) -> { x with template = v
                                    formatter = None }