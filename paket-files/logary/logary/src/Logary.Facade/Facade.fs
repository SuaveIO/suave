/// The logging namespace, which contains the logging abstraction for this
/// library. See https://github.com/logary/logary for details. This module is
/// completely stand-alone in that it has no external references and its adapter
/// in Logary has been well tested.
namespace Suave.Logging

open System
open System.Runtime.CompilerServices

/// The log level denotes how 'important' the gauge or event message is.
[<CustomEquality; CustomComparison>]
type LogLevel =
  /// The log message is not that important; can be used for intricate debugging.
  | Verbose
  /// The log message is at a default level, debug level. Useful for shipping to
  /// infrastructure that further processes it, but not so useful for human
  /// inspection in its raw format, except during development.
  | Debug
  /// The log message is informational; e.g. the service started, stopped or
  /// some important business event occurred.
  | Info
  /// The log message is a warning; e.g. there was an unhandled exception or
  /// an even occurred which was unexpected. Sometimes human corrective action
  /// is needed.
  | Warn
  /// The log message is at an error level, meaning an unhandled exception
  /// occurred at a location where it is deemed important to keeping the service
  /// running. A human should take corrective action.
  | Error
  /// The log message denotes a fatal error which cannot be recovered from. The
  /// service should be shut down. Human corrective action is needed.
  | Fatal

  /// Converts the LogLevel to a string
  override x.ToString () =
    match x with
    | Verbose -> "verbose"
    | Debug   -> "debug"
    | Info    -> "info"
    | Warn    -> "warn"
    | Error   -> "error"
    | Fatal   -> "fatal"

  /// Converts the string passed to a Loglevel.
  static member ofString (str : string) =
    if str = null then invalidArg "str" "may not be null"
    match str.ToLowerInvariant() with
    | "verbose" -> Verbose
    | "debug"   -> Debug
    | "info"    -> Info
    | "warn"    -> Warn
    | "error"   -> Error
    | "fatal"   -> Fatal
    | _         -> Info

  /// Turn the LogLevel into an integer
  member x.toInt () =
    (function
    | Verbose -> 1
    | Debug   -> 2
    | Info    -> 3
    | Warn    -> 4
    | Error   -> 5
    | Fatal   -> 6) x

  /// Turn an integer into a LogLevel
  static member ofInt i =
    (function
    | 1 -> Verbose
    | 2 -> Debug
    | 3 -> Info
    | 4 -> Warn
    | 5 -> Error
    | 6 -> Fatal
    | _ as i -> failwithf "LogLevel matching integer %i is not available" i) i

  interface IComparable<LogLevel> with
    member x.CompareTo other =
      compare (x.toInt()) (other.toInt())

  static member op_LessThan (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) < 0

  static member op_LessThanOrEqual (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) <= 0

  static member op_GreaterThan (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) > 0

  static member op_GreaterThanOrEqual (a, b) =
    (a :> IComparable<LogLevel>).CompareTo(b) >= 0

  override x.GetHashCode () =
    x.toInt ()

  interface IComparable with
    member x.CompareTo other =
      match other with
      | null ->
        1

      | :? LogLevel as tother ->
        (x :> IComparable<LogLevel>).CompareTo tother

      | _ ->
        failwithf "invalid comparison %A to %A" x other

  interface IEquatable<LogLevel> with
    member x.Equals other =
      x.toInt() = other.toInt()
  
  override x.Equals other =
    (x :> IComparable).CompareTo other = 0

/// Represents a logged value; either a Gauge or an Event.
type PointValue =
  /// An event is what it sounds like; something occurred and needs to be
  /// logged. Its field is named 'template' because it should not be interpolated
  /// with values; instead these values should be put in the 'fields' field of
  /// the Message.
  | Event of template:string
  /// This is as value for a metric, with a unit attached. The unit can be
  /// something like Seconds or Hz.
  | Gauge of value:int64 * units:string

/// The # of nanoseconds after 1970-01-01 00:00:00.
type EpochNanoSeconds = int64

/// Extensions to DateTime.
module DateTime =

  /// Get the Logary timestamp off the DateTime.
  let timestamp (dt : DateTime) : EpochNanoSeconds =
    (dt.Ticks - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks)
    * 100L

  /// Get the DateTimeOffset ticks off from the EpochNanoSeconds
  let ticksUTC (epoch : EpochNanoSeconds) : int64 =
    epoch / 100L
    + DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks

/// Extensions to DateTimeOffset.
module DateTimeOffset =

  /// Get the Logary timestamp off the DateTimeOffset.
  let timestamp (dt : DateTimeOffset) : EpochNanoSeconds =
    (dt.Ticks - DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks)
    * 100L

  /// Get the DateTimeOffset ticks from EpochNanoSeconds
  let ticksUTC (epoch : EpochNanoSeconds) : int64 =
    epoch / 100L
    + DateTimeOffset(1970, 1, 1, 0, 0, 0, TimeSpan.Zero).Ticks

/// This is record that is logged. It's capable of representing both metrics
/// (gauges) and events. See https://github.com/logary/logary for details.
type Message =
  { /// The 'path' or 'name' of this data point. Do not confuse template in
    /// (Event template) = message.value
    name      : string[]
    /// The main value for this metric or event. Either a Gauge or an Event. (A
    /// discriminated union type)
    value     : PointValue
    /// The semantic-logging data.
    fields    : Map<string, obj>
    /// When? nanoseconds since UNIX epoch.
    timestamp : EpochNanoSeconds
    /// How important? See the docs on the LogLevel type for details.
    level     : LogLevel }

  /// Gets the ticks for UTC since 0001-01-01 00:00:00 for this message. You
  /// can pass this value into a DateTimeOffset c'tor
  member x.utcTicks =
    DateTimeOffset.ticksUTC x.timestamp

  /// If you're looking for how to transform the Message's fields, then use the
  /// module methods rather than instance methods, since you'll be creating new
  /// values rather than changing an existing value.
  member x.README =
    ()

/// The logger is the interface for calling code to use for logging.
type Logger =
  /// Evaluates the callback if the log level is enabled. Returns an async that
  /// itself completes when the logging infrastructure has finished writing that
  /// Message. Completes directly if nothing is logged. What the ack means from
  /// a durability standpoint depends on the logging infrastructure you're using
  /// behind this facade. Will not block, besides doing the computation inside
  /// the callback. You should not do blocking operations in the callback.
  abstract member logWithAck : LogLevel -> (LogLevel -> Message) -> Async<unit>

  /// Evaluates the callback if the log level is enabled. Will not block,
  /// besides doing the computation inside the callback. You should not do
  /// blocking operations in the callback.
  abstract member log : LogLevel -> (LogLevel -> Message) -> unit

  /// Logs the message without awaiting the logging infrastructure's ack of
  /// having successfully written the log message. What the ack means from a
  /// durability standpoint depends on the logging infrastructure you're using
  /// behind this facade.
  abstract member logSimple : Message -> unit

/// Syntactic sugar on top of Logger.
[<AutoOpen>]
module internal LoggerEx =
  type Logger with
    member x.verbose (msgFactory : LogLevel -> Message) : unit =
      x.log Verbose msgFactory

    member x.debug (msgFactory : LogLevel -> Message) : unit =
      x.log Debug msgFactory

    member x.info msgFactory : unit =
      x.log Info msgFactory

    member x.warn msgFactory : unit =
      x.log Warn msgFactory

    member x.error msgFactory : unit =
      x.log Error msgFactory

    member x.fatal msgFactory : unit =
      x.log Fatal msgFactory

type LoggingConfig =
  { timestamp : unit -> int64
    getLogger : string[] -> Logger }

module internal Formatting =

  [<Literal>]
  let FieldExnKey = "exn"

  /// let the ISO8601 love flow
  let internal defaultFormatter (message : Message) =

    let formatLevel (level : LogLevel) =
      "[" + Char.ToUpperInvariant(level.ToString().[0]).ToString() + "] "

    let formatInstant (utcTicks : int64) =
      (DateTime(utcTicks, DateTimeKind.Utc).ToString("o")) + ": "

    let formatValue = function
      | Event template ->
        template

      | Gauge (value, units) ->
        sprintf "%i %s" value units

    let formatName (name : string[]) =
      " [" + String.concat "." name + "]"

    let formatExn (fields : Map<string, obj>) =
      match fields |> Map.tryFind FieldExnKey with
      | None ->
        String.Empty

      | Some ex ->
        " exn:\n" + ex.ToString()

    // [I] 2014-04-05T12:34:56Z: Hello World! [my.sample.app]
    formatLevel message.level +
    formatInstant message.utcTicks +
    formatValue message.value +
    formatName message.name +
    formatExn message.fields

/// Log a line with the given format, printing the current time in UTC ISO-8601 format
/// and then the string, like such:
/// '2013-10-13T13:03:50.2950037Z: today is the day'
type ConsoleWindowTarget(minLevel, ?formatter, ?colourise, ?originalColor, ?consoleSemaphore) =
  let sem           = defaultArg consoleSemaphore (obj())
  let originalColor = defaultArg originalColor Console.ForegroundColor
  let formatter     = defaultArg formatter Formatting.defaultFormatter
  let colourise     = defaultArg colourise true
  let write         = System.Console.WriteLine : string -> unit

  let toColour = function
    | LogLevel.Verbose -> ConsoleColor.DarkGreen
    | LogLevel.Debug   -> ConsoleColor.Green
    | LogLevel.Info    -> ConsoleColor.White
    | LogLevel.Warn    -> ConsoleColor.Yellow
    | LogLevel.Error   -> ConsoleColor.DarkRed
    | LogLevel.Fatal   -> ConsoleColor.Red

  let log color message =
    if colourise then
      lock sem <| fun _ ->
        Console.ForegroundColor <- color
        message |> formatter |> write
        Console.ForegroundColor <- originalColor
    else
      // we don't need to take another lock, since Console.WriteLine does that for us
      (write << formatter) message

  interface Logger with
    member x.logWithAck level msgFactory =
      if level >= minLevel then
        log (toColour level) (msgFactory level)
      async.Return ()

    member x.log level msgFactory =
      if level >= minLevel then
        log (toColour level) (msgFactory level)

    member x.logSimple msg =
      if msg.level >= minLevel then
        log (toColour msg.level) msg

type OutputWindowTarget(minLevel, ?formatter) =
  let formatter = defaultArg formatter Formatting.defaultFormatter
  let log msg = System.Diagnostics.Debug.WriteLine(formatter msg)

  interface Logger with
    member x.log level msgFactory =
      if level >= minLevel then log (msgFactory level)

    member x.logWithAck level msgFactory =
      if level >= minLevel then log (msgFactory level)
      async.Return ()

    member x.logSimple msg =
      if msg.level >= minLevel then log msg

/// A logger to use for combining a number of other loggers
type CombiningTarget(otherLoggers : Logger list) =
  let sendToAll level msgFactory =
    async {
      let! _ =
        otherLoggers
        |> List.map (fun l -> l.logWithAck level msgFactory)
        |> Async.Parallel
      return ()
    }

  interface Logger with
    member x.logWithAck level msgFactory =
      sendToAll level msgFactory

    member x.log level msgFactory =
      for logger in otherLoggers do
        logger.log level msgFactory

    member x.logSimple msg =
      sendToAll msg.level (fun _ -> msg)
      |> Async.Start

module Targets =

  let create level =
    if level >= LogLevel.Info then
      ConsoleWindowTarget(level) :> Logger
    else
      CombiningTarget(
        [ ConsoleWindowTarget(level)
          OutputWindowTarget(level) ])
      :> Logger

module Global =

  /// The global default configuration, which logs to Console at Info level.
  let DefaultConfig =
    { timestamp = fun () -> DateTimeOffset.timestamp DateTimeOffset.UtcNow
      getLogger = fun _ -> ConsoleWindowTarget(Info) :> Logger }

  let private config = ref DefaultConfig
  let private locker = obj ()

  /// The flyweight just references the current configuration. If you want
  /// multiple per-process logging setups, then don't use the static methods,
  /// but instead pass a Logger instance around, setting the name field of the
  /// Message value you pass into the logger.
  type internal Flyweight(name : string[]) =
    let initialLogger = (!config).getLogger name
    let mutable actualLogger : Logger option = None

    let withLogger action =
      let logger =
        if Object.ReferenceEquals(!config, DefaultConfig) then
          initialLogger
        elif actualLogger = None then
          lock locker <| fun _ ->
            if actualLogger = None then
              let logger' = (!config).getLogger name
              actualLogger <- Some logger'
              logger'
            else
              actualLogger |> Option.get
        else
          actualLogger |> Option.get

      action logger

    interface Logger with
      member x.log level msgFactory =
        withLogger (fun logger -> logger.log level msgFactory)

      member x.logWithAck level msgFactory =
        withLogger (fun logger -> logger.logWithAck level msgFactory)

      member x.logSimple message =
        withLogger (fun logger -> logger.logSimple message)

  let internal getStaticLogger (name : string []) =
    Flyweight name

  let timestamp () : EpochNanoSeconds =
    (!config).timestamp ()

  /// Call from the initialisation of your library. Initialises the
  /// Logary.Facade globally/per process.
  let initialise cfg =
    config := cfg

/// Module for acquiring static loggers (when you don't want or can't)
/// pass loggers as values.
module Log =

  /// Create a named logger. Full stop (.) acts as segment delimiter in the
  /// hierachy of namespaces and loggers.
  let create (name : string) =
    if name = null then invalidArg "name" "name is null"
    Global.getStaticLogger (name.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries))
    :> Logger

  /// Create an hierarchically named logger
  let createHiera (name : string[]) =
    if name = null then invalidArg "name" "name is null"
    if name.Length = 0 then invalidArg "name" "must have >0 segments"
    Global.getStaticLogger name
    :> Logger

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =

  /// Create a new event log message.
  let event level template =
    { name      = [||]
      value     = Event template
      fields    = Map.empty
      timestamp = Global.timestamp ()
      level     = level }

  /// Create a new event log message – like `event` but with parameters flipped.
  /// Useful to use with `Logger.log` with point-free style, to reduce the
  /// noise.
  let eventX template level =
    event level template

  /// Create a new instantaneous value in a log message.
  let gauge value units =
    { name      = [||]
      value     = Gauge (value, units)
      fields    = Map.empty
      timestamp = Global.timestamp ()
      level     = Debug }

  /// Sets the name/path of the log message.
  let setName (name : string[]) (x : Message) =
    { x with name = name }

  /// Sets the name as a single string; if this string contains dots, the string
  /// will be split on these dots.
  let setSingleName (name : string) (x : Message) =
    if name = null then invalidArg "name" "may not be null"

    let name' =
      name.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries)

    x |> setName name'

  /// Sets the value of the field on the log message.
  let setFieldValue (key : string) (value : obj) (x : Message) =
    let put k v m =
      match m |> Map.tryFind k with
      | None ->
        m |> Map.add k v

      | Some _ ->
        m |> Map.remove k |> Map.add k v

    { x with fields = x.fields |> put key value }

  /// Sets the timestamp on the log message.
  let setTimestamp (ts : EpochNanoSeconds) (x : Message) =
    { x with timestamp = ts }

  /// Sets the level on the log message.
  let setLevel (level : LogLevel) (x : Message) =
    { x with level = level }

  /// Adds an exception to the Message, to the 'errors' field, inside a list.
  let addExn ex (x : Message) =
    let fields' =
      match Map.tryFind "errors" x.fields with
      | None ->
        x.fields |> Map.add "errors" (box [ box ex ])
      
      | Some errors ->
        let arr : obj list = unbox errors
        x.fields |> Map.remove "errors" |> Map.add "errors" (box (box ex :: arr))

    { x with fields = fields' }
