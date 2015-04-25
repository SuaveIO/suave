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
      | LogLevel.Verbose -> ConsoleColor.DarkGreen
      | LogLevel.Debug -> ConsoleColor.Green
      | LogLevel.Info -> ConsoleColor.White
      | LogLevel.Warn -> ConsoleColor.Yellow
      | LogLevel.Error -> ConsoleColor.DarkRed
      | LogLevel.Fatal -> ConsoleColor.Red

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

  let saneDefaultsFor level =
    if level >= LogLevel.Warn then
      ConsoleWindowLogger(level) :> Logger
    else
      CombiningLogger(
        [ ConsoleWindowLogger(level)
          OutputWindowLogger(level) ]) :> Logger

  [<System.Obsolete("Use saneDefaultsFor")>]
  /// Obsolete
  let sane_defaults_for level = saneDefaultsFor level
