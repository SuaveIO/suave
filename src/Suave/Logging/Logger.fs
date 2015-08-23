namespace Suave.Logging

open System

/// The primary Logger abstraction that you can log data into
type Logger =
  /// log - evaluate the function if the log level matches - by making it
  /// a function we don't needlessly need to evaluate it
  /// Calls to this method must be thread-safe and not change any state
  abstract member Log : LogLevel -> (unit -> LogLine) -> unit

module Loggers =
  /// A logger to use for combining a number of other loggers
  type CombiningLogger(otherLoggers : Logger list) =
    interface Logger with
      member x.Log level fLine =
        otherLoggers |> List.iter (fun l -> l.Log level fLine)

  /// let the ISO8601 love flow
  let internal defaultFormatter (line : LogLine) =
    // [I] 2014-04-05T12:34:56Z: Hello World! [my.sample.app]
    "[" + Char.ToUpperInvariant(line.level.ToString().[0]).ToString() + "] " +
    (DateTime(line.tsUTCTicks, DateTimeKind.Utc).ToString("o")) + ": " +
    line.formatMessage() + " [" + line.path + "]" +
    (match line.``exception`` with | Some e -> " exn:\n" + e.ToString() | None -> "")

  /// Log a line with the given format, printing the current time in UTC ISO-8601 format
  /// and then the string, like such:
  /// '2013-10-13T13:03:50.2950037Z: today is the day'
  type ConsoleWindowLogger(minLevel, ?formatter, ?colourise, ?originalColor, ?consoleSemaphore) =
    let sem           = defaultArg consoleSemaphore (obj())
    let originalColor = defaultArg originalColor Console.ForegroundColor
    let formatter     = defaultArg formatter defaultFormatter
    let colourise     = defaultArg colourise true
    let write         = System.Console.WriteLine : string -> unit

    let toColor = function
      | LogLevel.Verbose -> ConsoleColor.DarkGreen
      | LogLevel.Debug   -> ConsoleColor.Green
      | LogLevel.Info    -> ConsoleColor.White
      | LogLevel.Warn    -> ConsoleColor.Yellow
      | LogLevel.Error   -> ConsoleColor.DarkRed
      | LogLevel.Fatal   -> ConsoleColor.Red

    let log color line =
      if colourise then
        lock sem <| fun _ ->
          Console.ForegroundColor <- color
          (write << formatter) line
          Console.ForegroundColor <- originalColor
      else
        // we don't need to take another lock, since Console.WriteLine does that for us
        (write << formatter) line

    interface Logger with
      member x.Log level f = if level >= minLevel then log (toColor level) (f ())
      
  type OutputWindowLogger(minLevel, ?formatter) =
    let formatter = defaultArg formatter defaultFormatter
    let log line = System.Diagnostics.Debug.WriteLine(formatter line)
    interface Logger with
      member x.Log level fLine = if level >= minLevel then log (fLine ())

  let saneDefaultsFor level =
    if level >= LogLevel.Warn then
      ConsoleWindowLogger(level) :> Logger
    else
      CombiningLogger(
        [ ConsoleWindowLogger(level)
          OutputWindowLogger(level) ]) :> Logger
