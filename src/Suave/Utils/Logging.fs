/// The logging namespace, which contains the logging abstraction for this
/// library.
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

/// Helper functions for transforming DateTime to timestamps in unix epoch.
module DateTime =

  /// Get the Logary timestamp off the DateTime.
  let timestamp (dt : DateTime) : EpochNanoSeconds =
    (dt.Ticks - DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks)
    * 100L

  /// Get the DateTimeOffset ticks off from the EpochNanoSeconds.
  let ticksUTC (epoch : EpochNanoSeconds) : int64 =
    epoch / 100L
    + DateTime(1970, 1, 1, 0, 0, 0, DateTimeKind.Utc).Ticks

/// Helper functions for transforming DateTimeOffset to timestamps in unix epoch.
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
    /// The structured-logging data.
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

/// The logger is the interface for calling code to use for logging. Its
/// different functions have different semantics - read the docs for each
/// method to choose the right one for your use-case.
type Logger =
  /// Gets the name of the logger instance.
  abstract member name : string[]
  /// Logs with the specified log level with backpressure via the logging
  /// library's buffers *and* ACK/flush to the underlying message targets.
  ///
  /// Calls to this function will block the caller only while executing the
  /// callback (if the level is active).
  ///
  /// The returned async value will yield when the message has been flushed to
  /// the underlying message targets.
  ///
  /// You need to start the (cold) async value for the logging to happen.
  ///
  /// You should not do blocking/heavy operations in the callback.
  abstract member logWithAck : LogLevel -> (LogLevel -> Message) -> Async<unit>

  /// Logs with the specified log level with backpressure via the logging
  /// library's buffers.
  ///
  /// Calls to this function will block the caller only while executing the
  /// callback (if the level is active).
  ///
  /// The returned async value will yield when the message has been added to
  /// the buffers of the logging library.
  ///
  /// You need to start the (cold) async value for the logging to happen.
  ///
  /// You should not do blocking/heavy operations in the callback.
  abstract member log : LogLevel -> (LogLevel -> Message) -> unit

/// Syntactic sugar on top of Logger for F# libraries.
[<AutoOpen>]
module LoggerEx =
  open System.Diagnostics
  
  type Logger with

    [<Conditional("DEBUG")>]
    member x.verbose (messageFactory : LogLevel -> Message) : unit =
      x.log Verbose messageFactory

    [<Conditional("DEBUG")>]
    member x.debug (messageFactory : LogLevel -> Message) : unit =
      x.log LogLevel.Debug messageFactory

    member x.info (messageFactory : LogLevel -> Message) : unit =
      x.log Info messageFactory

    member x.warn (messageFactory : LogLevel -> Message) : unit =
      x.log Warn messageFactory

    member x.error (messageFactory : LogLevel -> Message) : unit =
      x.log Error messageFactory

    member x.fatal (messageFactory : LogLevel -> Message) : unit =
      x.log Fatal messageFactory

    member x.logSimple message : unit =
      x.log message.level (fun _ -> message)

type LoggingConfig =
  { /// The `timestamp` function should preferably be monotonic and not 'jumpy'
    /// or take much time to call.
    timestamp        : unit -> int64
    /// The `getLogger` function returns a logger that directly can be logged to.
    getLogger        : string[] -> Logger
    /// When composing apps from the outside-in (rather than having a unified
    /// framework with static/global config) with libraries (again, rather than
    /// a unified framework) like is best-practice, there's not necessarily a
    /// way to coordinate around the STDOUT and STDERR streams between
    /// different libraries running things on different threads. Use Logary's
    /// adapter to replace this semaphore with a global semaphore.
    consoleSemaphore : obj }

module Literate =
  /// The output tokens, which can be potentially coloured.
  type LiterateToken =
    | Text | Subtext
    | Punctuation
    | LevelVerbose | LevelDebug | LevelInfo | LevelWarning | LevelError | LevelFatal
    | KeywordSymbol | NumericSymbol | StringSymbol | OtherSymbol | NameSymbol
    | MissingTemplateField

  type LiterateOptions =
    { formatProvider          : IFormatProvider
      theme                   : LiterateToken -> ConsoleColor
      getLogLevelText         : LogLevel -> string
      printTemplateFieldNames : bool }

    static member create ?formatProvider =
      // note: literate is meant for human consumption, and so the default
      // format provider of 'Current' is appropriate here. The reader expects
      // to see the dates, numbers, currency, etc formatted in the local culture
      { formatProvider = defaultArg formatProvider Globalization.CultureInfo.CurrentCulture
        getLogLevelText = function
                | Debug ->    "DBG"
                | Error ->    "ERR"
                | Fatal ->    "FTL"
                | Info ->     "INF"
                | Verbose ->  "VRB"
                | Warn ->     "WRN"
        theme = function
                | Text -> ConsoleColor.White
                | Subtext -> ConsoleColor.Gray
                | Punctuation -> ConsoleColor.DarkGray
                | LevelVerbose -> ConsoleColor.Gray
                | LevelDebug -> ConsoleColor.Gray
                | LevelInfo -> ConsoleColor.White
                | LevelWarning -> ConsoleColor.Yellow
                | LevelError -> ConsoleColor.Red
                | LevelFatal -> ConsoleColor.Red
                | KeywordSymbol -> ConsoleColor.Blue
                | NumericSymbol -> ConsoleColor.Magenta
                | StringSymbol -> ConsoleColor.Cyan
                | OtherSymbol -> ConsoleColor.Green
                | NameSymbol -> ConsoleColor.Gray
                | MissingTemplateField -> ConsoleColor.Red
        printTemplateFieldNames = false }

    static member createInvariant() =
      LiterateOptions.create Globalization.CultureInfo.InvariantCulture

/// Module that contains the 'known' keys of the Maps in the Message type's
/// fields/runtime data.
module Literals =

  /// What version of the Facade is this. This is a major version that allows the Facade
  /// adapter to choose how it handles the API.
  let FacadeVersion = 2u

  /// What language this Facade has. This controls things like naming standards.
  let FacadeLanguage = "F#"

  [<Literal>]
  let FieldExnKey = "exn"

  [<Literal>]
  let FieldErrorsKey = "errors"

module internal FsMtParser =
  open System.Text

  type Property(name : string, format : string) =
    static let emptyInstance = Property("", null)
    static member empty = emptyInstance
    member x.name = name
    member x.format = format
    member internal x.AppendPropertyString(sb : StringBuilder, ?replacementName) =
      sb.Append("{")
        .Append(defaultArg replacementName name)
        .Append(match x.format with null | "" -> "" | _ -> ":" + x.format)
        .Append("}")
    override x.ToString() = x.AppendPropertyString(StringBuilder()).ToString()

  module internal ParserBits =

    let inline isNull o =
      match o with
      | null -> true
      | _ -> false

    let inline isLetterOrDigit c = System.Char.IsLetterOrDigit c
    let inline isValidInPropName c = c = '_' || System.Char.IsLetterOrDigit c
    let inline isValidInFormat c = c <> '}' && (c = ' ' || isLetterOrDigit c || System.Char.IsPunctuation c)
    let inline isValidCharInPropTag c = c = ':' || isValidInPropName c || isValidInFormat c

    [<Struct>]
    type Range(startIndex : int, endIndex : int) =
      member inline x.start = startIndex
      member inline x.``end`` = endIndex
      member inline x.length = (endIndex - startIndex) + 1
      member inline x.getSubstring (s : string) = s.Substring(startIndex, x.length)
      member inline x.isEmpty = startIndex = -1 && endIndex = -1
      static member inline substring (s : string, startIndex, endIndex) = s.Substring(startIndex, (endIndex - startIndex) + 1)
      static member inline empty = Range(-1, -1)

    let inline tryGetFirstCharInRange predicate (s : string) (range : Range) =
      let rec go i =
        if i > range.``end`` then -1
        else if not (predicate s.[i]) then go (i+1) else i
      go range.start

    let inline tryGetFirstChar predicate (s : string) first =
      tryGetFirstCharInRange predicate s (Range(first, s.Length - 1))

    let inline hasAnyInRange predicate (s : string) (range : Range) =
      match tryGetFirstChar (predicate) s range.start with
      | -1 ->
        false
      | i ->
        i <= range.``end``

    let inline hasAny predicate (s : string) = hasAnyInRange predicate s (Range(0, s.Length - 1))
    let inline indexOfInRange s range c = tryGetFirstCharInRange ((=) c) s range

    let inline tryGetPropInRange (template : string) (within : Range) : Property =
      // Attempts to validate and parse a property token within the specified range inside
      // the template string. If the property insides contains any invalid characters,
      // then the `Property.Empty' instance is returned (hence the name 'try')
      let nameRange, formatRange =
        match indexOfInRange template within ':' with
        | -1 ->
          within, Range.empty // no format
        | formatIndex ->
          Range(within.start, formatIndex-1), Range(formatIndex+1, within.``end``) // has format part
      let propertyName = nameRange.getSubstring template
      if propertyName = "" || (hasAny (not<<isValidInPropName) propertyName) then
        Property.empty
      elif (not formatRange.isEmpty) && (hasAnyInRange (not<<isValidInFormat) template formatRange) then
        Property.empty
      else
        let format = if formatRange.isEmpty then null else formatRange.getSubstring template
        Property(propertyName, format)

    let findNextNonPropText (startAt : int) (template : string) (foundText : string->unit) : int =
      // Finds the next text token (starting from the 'startAt' index) and returns the next character
      // index within the template string. If the end of the template string is reached, or the start
      // of a property token is found (i.e. a single { character), then the 'consumed' text is passed
      // to the 'foundText' method, and index of the next character is returned.
      let mutable escapedBuilder = Unchecked.defaultof<StringBuilder> // don't create one until it's needed
      let inline append (ch : char) = if not (isNull escapedBuilder) then escapedBuilder.Append(ch) |> ignore
      let inline createStringBuilderAndPopulate i =
        if isNull escapedBuilder then
          escapedBuilder <- StringBuilder() // found escaped open-brace, take the slow path
          for chIndex = startAt to i-1 do append template.[chIndex] // append all existing chars
      let rec go i =
        if i >= template.Length then
          template.Length // bail out at the end of the string
        else
          let ch = template.[i]
          match ch with
          | '{' ->
            if (i+1) < template.Length && template.[i+1] = '{' then
              createStringBuilderAndPopulate i; append ch; go (i+2)
            else i // found an open brace (potentially a property), so bail out
          | '}' when (i+1) < template.Length && template.[i+1] = '}' ->
            createStringBuilderAndPopulate i; append ch; go (i+2)
          | _ ->
            append ch; go (i+1)

      let nextIndex = go startAt
      if (nextIndex > startAt) then // if we 'consumed' any characters, signal that we 'foundText'
        if isNull escapedBuilder then
          foundText (Range.substring(template, startAt, nextIndex - 1))
        else
          foundText (escapedBuilder.ToString())
      nextIndex

    let findPropOrText (start : int) (template : string)
                       (foundText : string -> unit)
                       (foundProp : Property -> unit) : int =
      // Attempts to find the indices of the next property in the template
      // string (starting from the 'start' index). Once the start and end of
      // the property token is known, it will be further validated (by the
      // tryGetPropInRange method). If the range turns out to be invalid, it's
      // not a property token, and we return it as text instead. We also need
      // to handle some special case here: if the end of the string is reached,
      // without finding the close brace (we just signal 'foundText' in that case).
      let nextInvalidCharIndex =
        match tryGetFirstChar (not << isValidCharInPropTag) template (start+1) with
        | -1 ->
          template.Length
        | idx ->
          idx

      if nextInvalidCharIndex = template.Length || template.[nextInvalidCharIndex] <> '}' then
        foundText (Range.substring(template, start, (nextInvalidCharIndex - 1)))
        nextInvalidCharIndex
      else
        let nextIndex = nextInvalidCharIndex + 1
        let propInsidesRng = Range(start + 1, nextIndex - 2)
        match tryGetPropInRange template propInsidesRng with
        | prop when not (obj.ReferenceEquals(prop, Property.empty)) ->
          foundProp prop
        | _ ->
          foundText (Range.substring(template, start, (nextIndex - 1)))
        nextIndex

  /// Parses template strings such as "Hello, {PropertyWithFormat:##.##}"
  /// and calls the 'foundTextF' or 'foundPropF' functions as the text or
  /// property tokens are encountered.
  let parseParts (template : string) foundTextF foundPropF =
    let tlen = template.Length
    let rec go start =
      if start >= tlen then () else
      match ParserBits.findNextNonPropText start template foundTextF with
      | next when next <> start ->
        go next
      | _ ->
        go (ParserBits.findPropOrText start template foundTextF foundPropF)
    go 0

/// Internal module for formatting text for printing to the console.
module internal Formatting =
  open System.Text
  open Literals
  open Literate

  let literateFormatValue (options : LiterateOptions) (fields : Map<string, obj>) = function
    | Event template ->
      let themedParts = ResizeArray<string * LiterateToken>()
      let matchedFields = ResizeArray<string>()
      let foundText (text: string) = themedParts.Add (text, Text)
      let foundProp (prop: FsMtParser.Property) =
        match Map.tryFind prop.name fields with
        | Some propValue ->
          // render using string.Format, so the formatting is applied
          let stringFormatTemplate = prop.AppendPropertyString(StringBuilder(), "0").ToString()
          let fieldAsText = String.Format (options.formatProvider, stringFormatTemplate, [| propValue |])
          // find the right theme colour based on data type
          let valueColour =
            match propValue with
            | :? bool ->
              KeywordSymbol
            | :? int16 | :? int32 | :? int64 | :? decimal | :? float | :? double ->
              NumericSymbol
            | :? string | :? char ->
              StringSymbol
            | _ ->
              OtherSymbol
          if options.printTemplateFieldNames then
            themedParts.Add ("["+prop.name+"] ", Subtext)
          matchedFields.Add prop.name
          themedParts.Add (fieldAsText, valueColour)

        | None ->
          themedParts.Add (prop.ToString(), MissingTemplateField)

      FsMtParser.parseParts template foundText foundProp
      Set.ofSeq matchedFields, List.ofSeq themedParts

    | Gauge (value, units) ->
      Set.empty, [ sprintf "%i" value, NumericSymbol
                   sprintf "%s" units, KeywordSymbol ]

  let formatValue (fields : Map<string, obj>) (pv : PointValue) =
    let matchedFields, themedParts =
      literateFormatValue (LiterateOptions.createInvariant()) fields pv
    matchedFields, System.String.Concat(themedParts |> List.map fst)

  let literateExceptionColouriser (options : LiterateOptions) (ex : exn) =
    let stackFrameLinePrefix = "   at" // 3 spaces
    let monoStackFrameLinePrefix = "  at" // 2 spaces
    use exnLines = new System.IO.StringReader(ex.ToString())
    let rec go lines =
      match exnLines.ReadLine() with
      | null ->
        List.rev lines // finished reading
      | line ->
        if line.StartsWith(stackFrameLinePrefix) || line.StartsWith(monoStackFrameLinePrefix) then
          // subtext
          go ((line, Subtext) :: (Environment.NewLine, Text) :: lines)
        else
          // regular text
          go ((line, Text) :: (Environment.NewLine, Text) :: lines)
    go []

  let literateColouriseExceptions (context : LiterateOptions) message =
    let exnExceptionParts =
      match message.fields.TryFind FieldExnKey with
      | Some (:? Exception as ex) ->
        literateExceptionColouriser context ex
      | _ ->
        [] // there is no spoon
    let errorsExceptionParts =
      match message.fields.TryFind FieldErrorsKey with
      | Some (:? List<obj> as exnListAsObjList) ->
        exnListAsObjList |> List.collect (function
          | :? exn as ex ->
            literateExceptionColouriser context ex
          | _ ->
            [])
      | _ ->
        []

    exnExceptionParts @ errorsExceptionParts

  let getLogLevelToken = function
    | Verbose -> LevelVerbose
    | Debug -> LevelDebug
    | Info -> LevelInfo
    | Warn -> LevelWarning
    | Error -> LevelError
    | Fatal -> LevelFatal

  /// Split a structured message up into theme-able parts (tokens), allowing the
  /// final output to display to a user with colours to enhance readability.
  let literateDefaultTokeniser (options : LiterateOptions) (message : Message) : (string * LiterateToken) list =
    let formatLocalTime (utcTicks : int64) =
      DateTimeOffset(utcTicks, TimeSpan.Zero).LocalDateTime.ToString("HH:mm:ss", options.formatProvider),
      Subtext

    let themedMessageParts =
      message.value |> literateFormatValue options message.fields |> snd

    let themedExceptionParts = literateColouriseExceptions options message

    [ "[", Punctuation
      formatLocalTime message.utcTicks
      " ", Subtext
      options.getLogLevelText message.level, getLogLevelToken message.level
      "] ", Punctuation ]
    @ themedMessageParts
    @ themedExceptionParts

  let literateDefaultColourWriter sem (parts : (string * ConsoleColor) list) =
    lock sem <| fun _ ->
      let originalColour = Console.ForegroundColor
      let mutable currentColour = originalColour
      parts |> List.iter (fun (text, colour) ->
        if currentColour <> colour then
          Console.ForegroundColor <- colour
          currentColour <- colour
        Console.Write(text)
      )
      if currentColour <> originalColour then
        Console.ForegroundColor <- originalColour

  /// let the ISO8601 love flow
  let defaultFormatter (message : Message) =
    let app (x : obj) (sb : StringBuilder) =
      sb.Append x |> ignore

    let formatLevel (level : LogLevel) =
      "[" + Char.ToUpperInvariant(level.ToString().[0]).ToString() + "] "

    let formatInstant (utcTicks : int64) =
      (DateTimeOffset(utcTicks, TimeSpan.Zero).ToString("o")) + ": "

    let formatName (name : string[]) =
      " [" + String.concat "." name + "]"

    let formatExn (fields : Map<string, obj>) =
      match fields |> Map.tryFind FieldExnKey with
      | None ->
        String.Empty
      | Some ex ->
        " exn:\n" + ex.ToString()

    let formatFields (ignored : Set<string>) (fields : Map<string, obj>) =
      if not (Map.isEmpty fields) then
        fields
        |> Seq.filter (fun (KeyValue (k, _)) ->
          not (ignored |> Set.contains k))
        |> Seq.map (fun (KeyValue (k, v)) ->
          sprintf "\n - %s: %O" k v)
        |> String.concat ""
      else
        ""

    let matchedFields, valueString =
      formatValue message.fields message.value

    // [I] 2014-04-05T12:34:56Z: Hello World! [my.sample.app]
    formatLevel message.level +
    formatInstant message.utcTicks +
    valueString +
    formatName message.name +
    formatExn message.fields +
    formatFields matchedFields message.fields

/// Assists with controlling the output of the `LiterateConsoleTarget`.
module internal LiterateFormatting =
  open Literate
  type TokenisedPart = string * LiterateToken
  type LiterateTokeniser = LiterateOptions -> Message -> TokenisedPart list

  type internal TemplateToken = TextToken of text:string | PropToken of name : string * format : string
  let internal parseTemplate template =
    let tokens = ResizeArray<TemplateToken>()
    let foundText (text: string) = tokens.Add (TextToken text)
    let foundProp (prop: FsMtParser.Property) = tokens.Add (PropToken (prop.name, prop.format))
    FsMtParser.parseParts template foundText foundProp
    tokens :> seq<TemplateToken>
  
  [<AutoOpen>]
  module OutputTemplateTokenisers =

    let tokeniseTimestamp format (options : LiterateOptions) (message : Message) = 
      let localDateTimeOffset = DateTimeOffset(message.utcTicks, TimeSpan.Zero).ToLocalTime()
      let formattedTimestamp = localDateTimeOffset.ToString(format, options.formatProvider)
      seq { yield formattedTimestamp, Subtext }

    let tokeniseTimestampUtc format (options : LiterateOptions) (message : Message) = 
      let utcDateTimeOffset = DateTimeOffset(message.utcTicks, TimeSpan.Zero)
      let formattedTimestamp = utcDateTimeOffset.ToString(format, options.formatProvider)
      seq { yield formattedTimestamp, Subtext }

    let tokeniseMissingField name format =
      seq {
        yield "{", Punctuation
        yield name, MissingTemplateField
        if not (String.IsNullOrEmpty format) then
          yield ":", Punctuation
          yield format, Subtext
        yield "}", Punctuation }

    let tokeniseLogLevel (options : LiterateOptions) (message : Message) =
      seq { yield options.getLogLevelText message.level, Formatting.getLogLevelToken message.level }

    let tokeniseSource (options : LiterateOptions) (message : Message) =
      seq { yield (String.concat "." message.name), Subtext }

    let tokeniseNewline (options : LiterateOptions) (message : Message) =
      seq { yield Environment.NewLine, Text }

    let tokeniseTab (options : LiterateOptions) (message : Message) =
      seq { yield "\t", Text }

  /// Creates a `LiterateTokeniser` function which can be passed to the `LiterateConsoleTarget`
  /// constructor in order to customise how each log message is rendered. The default template
  /// would be: `[{timestampLocal:HH:mm:ss} {level}] {message}{newline}{exceptions}`.
  /// Available template fields are: `timestamp`, `timestampUtc`, `level`, `source`,
  /// `newline`, `tab`, `message`, `exceptions`. Any misspelled or otheriwese invalid property
  /// names will be treated as `LiterateToken.MissingTemplateField`. 
  let tokeniserForOutputTemplate template : LiterateTokeniser =
    let tokens = parseTemplate template
    fun options message ->
      seq {
        for token in tokens do
          match token with
          | TextToken text -> yield text, LiterateToken.Punctuation
          | PropToken (name, format) ->
            match name with
            | "timestamp" ->    yield! tokeniseTimestamp format options message
            | "timestampUtc" -> yield! tokeniseTimestampUtc format options message
            | "level" ->        yield! tokeniseLogLevel options message
            | "source" ->       yield! tokeniseSource options message
            | "newline" ->      yield! tokeniseNewline options message
            | "tab" ->          yield! tokeniseTab options message
            | "message" ->      yield! Formatting.literateFormatValue options message.fields message.value |> snd
            | "exceptions" ->   yield! Formatting.literateColouriseExceptions options message
            | _ ->              yield! tokeniseMissingField name format
      }
      |> Seq.toList

/// Logs a line in a format that is great for human consumption,
/// using console colours to enhance readability.
/// Sample: [10:30:49 INF] User "AdamC" began the "checkout" process with 100 cart items
type LiterateConsoleTarget(name, minLevel, ?options, ?literateTokeniser, ?outputWriter, ?consoleSemaphore) =
  let sem          = defaultArg consoleSemaphore (obj())
  let options      = defaultArg options (Literate.LiterateOptions.create())
  let tokenise     = defaultArg literateTokeniser Formatting.literateDefaultTokeniser
  let colourWriter = defaultArg outputWriter Formatting.literateDefaultColourWriter sem

  let colouriseThenNewLine message =
    (tokenise options message) @ [Environment.NewLine, Literate.Text]
    |> List.map (fun (s, t) ->
      s, options.theme(t))

  /// Creates the target with a custom output template. The default `outputTemplate`
  /// is `[{timestampLocal:HH:mm:ss} {level}] {message}{exceptions}`.
  /// Available template fields are: `timestamp`, `timestampUtc`, `level`, `source`,
  /// `newline`, `tab`, `message`, `exceptions`. Any misspelled or otheriwese invalid property
  /// names will be treated as `LiterateToken.MissingTemplateField`.
  new (name, minLevel, outputTemplate, ?options, ?outputWriter, ?consoleSemaphore) =
    let tokeniser = LiterateFormatting.tokeniserForOutputTemplate outputTemplate
    LiterateConsoleTarget(name, minLevel, ?options=options, literateTokeniser=tokeniser, ?outputWriter=outputWriter, ?consoleSemaphore=consoleSemaphore)

  interface Logger with
    member x.name = name
    member x.logWithAck level msgFactory =
      if level >= minLevel then
        colourWriter (colouriseThenNewLine (msgFactory level))
      async.Return ()
    member x.log level msgFactory =
      if level >= minLevel then
        colourWriter (colouriseThenNewLine (msgFactory level))

type TextWriterTarget(name, minLevel, writer : System.IO.TextWriter, ?formatter) =
  let formatter = defaultArg formatter Formatting.defaultFormatter
  let log msg = writer.WriteLine(formatter msg)

  interface Logger with
    member x.name = name
    member x.log level messageFactory =
      if level >= minLevel then log (messageFactory level)

    member x.logWithAck level messageFactory =
      if level >= minLevel then log (messageFactory level)
      async.Return ()

type OutputWindowTarget(name, minLevel, ?formatter) =
  let formatter = defaultArg formatter Formatting.defaultFormatter
  let log msg = System.Diagnostics.Debug.WriteLine(formatter msg)

  interface Logger with
    member x.name = name
    member x.log level messageFactory =
      if level >= minLevel then log (messageFactory level)

    member x.logWithAck level messageFactory =
      if level >= minLevel then log (messageFactory level)
      async.Return ()

/// A logger to use for combining a number of other loggers
type CombiningTarget(name, otherLoggers : Logger list) =
  interface Logger with
    member x.name = name
    member x.logWithAck level messageFactory =
      otherLoggers
      |> List.map (fun l -> l.logWithAck level messageFactory)
      |> Async.Parallel
      |> Async.Ignore // Async<unit>

    member x.log level messageFactory =
      otherLoggers
      |> List.map (fun l -> l.log level messageFactory)
      |> ignore

module Global =
  /// This is the global semaphore for colourising the console output. Ensure
  /// that the same semaphore is used across libraries by using the Logary
  /// Facade Adapter in the final composing app/service.
  let private consoleSemaphore = obj ()

  /// The global default configuration, which logs to Console at Info level.
  let defaultConfig =
    { timestamp        = fun () -> DateTimeOffset.timestamp DateTimeOffset.UtcNow
      getLogger        = fun name -> LiterateConsoleTarget(name, Info) :> Logger
      consoleSemaphore = consoleSemaphore }

  let private config =
    ref (defaultConfig, (* logical clock *) 1u)

  /// The flyweight just references the current configuration. If you want
  /// multiple per-process logging setups, then don't use the static methods,
  /// but instead pass a Logger instance around, setting the name field of the
  /// Message value you pass into the logger.
  type internal Flyweight(name : string[]) =
    let updating = obj()
    let mutable fwClock : uint32 = snd !config
    let mutable logger : Logger = (fst !config).getLogger name
    let rec withLogger action =
      let cfg, cfgClock = !config // copy to local
      let fwCurr = fwClock // copy to local
      if cfgClock <> fwCurr then
        lock updating <| fun _ ->
          logger <- cfg.getLogger name
          fwClock <- fwCurr + 1u
      action logger

    let ensureName (m : Message) =
      if Array.isEmpty m.name then { m with name = name } else m

    interface Logger with
      member x.name = name
      member x.log level msgFactory =
        withLogger (fun logger -> logger.log level (msgFactory >> ensureName))

      member x.logWithAck level msgFactory =
        withLogger (fun logger -> logger.logWithAck level (msgFactory >> ensureName))

  let internal getStaticLogger (name : string []) =
    Flyweight name

  let timestamp () : EpochNanoSeconds =
    (fst !config).timestamp ()

  /// Returns the synchronisation object to use when printing to the console.
  let internal semaphore () =
    (fst !config).consoleSemaphore

  /// Run the passed function under the console semaphore lock.
  let internal lockSem fn =
    lock (semaphore ()) fn

  /// Call from the initialisation of your library. Initialises the
  /// Suave.Logging globally/per process.
  let initialise cfg =
    config := (cfg, snd !config + 1u)

  let initialiseIfDefault cfg =
    if snd !config = 1u then initialise cfg

/// "Shortcut" for creating targets; useful at the top-level configuration point of
/// your library.
module Targets =
  /// Create a new target. Prefer `Log.create` in your own libraries, or let the
  /// composing app replace your target instance through your configuration.
  ///
  /// Will log to console (colourised) by default, and also to the output window
  /// in your IDE if you specify a level below Info.
  let create level name =
    if level >= LogLevel.Info then
      LiterateConsoleTarget(name, level, consoleSemaphore = Global.semaphore()) :> Logger
    else
      CombiningTarget(
        name,
        [ LiterateConsoleTarget(name, level, consoleSemaphore = Global.semaphore())
          OutputWindowTarget(name, level) ])
      :> Logger

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

/// The Message module contains functions that can help callers compose messages. This
/// module is especially helpful to open to make calls into Logary's facade small.
[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Message =
  open Literals

  /// Create a new event log message.
  let event level template =
    { name      = [||]
      value     = Event template
      fields    = Map.empty
      timestamp = Global.timestamp ()
      level     = level }

  /// Create a new event log message – like `event` but with parameters flipped.
  /// Useful to use with `Logger.log` with point-free style, to reduce the
  /// noise. E.g. `logger.logVerbose (eventX "Returned {code}" >> setField "code" 24)`
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

  /// Sets the final portion o fthe name of the Message.
  let setNameEnding (ending : string) (x : Message) =
    if String.IsNullOrWhiteSpace ending then x else
    let segs = ResizeArray<_>(x.name)
    segs.Add ending
    { x with name = segs.ToArray() }

  /// Sets the name as a single string; if this string contains dots, the string
  /// will be split on these dots.
  let setSingleName (name : string) (x : Message) =
    if name = null then invalidArg "name" "may not be null"

    let name' =
      name.Split([|'.'|], StringSplitOptions.RemoveEmptyEntries)

    x |> setName name'

  /// Sets the value of the field on the log message.
  let setField (key : string) (value : obj) (x : Message) =
    { x with fields = x.fields |> Map.add key value }

  /// Alias to `setField`
  let setFieldValue = setField

  /// Sets the timestamp on the log message.
  let setTimestamp (ts : EpochNanoSeconds) (x : Message) =
    { x with timestamp = ts }

  /// Sets the level on the log message.
  let setLevel (level : LogLevel) (x : Message) =
    { x with level = level }

  /// Adds an exception to the Message, to the 'errors' field, inside a list.
  let addExn ex (x : Message) =
    let fields' =
      match Map.tryFind FieldErrorsKey x.fields with
      | None ->
        x.fields |> Map.add FieldErrorsKey (box [ box ex ])

      | Some errors ->
        let arr : obj list = unbox errors
        x.fields |> Map.add FieldErrorsKey (box (box ex :: arr))

    { x with fields = fields' }
