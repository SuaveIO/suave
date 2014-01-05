module Suave.Log

open System
open System.Diagnostics

type LogLevel =
  /// The most verbose log level, more verbose than Debug.
  | Verbose = 0
  /// Less verbose than Verbose, more verbose than Info
  | Debug = 1
  /// Less verbose than Debug, more verbose than Warn
  | Info = 2
  /// Less verbose than Info, more verbose than Error
  | Warn = 3
  /// Less verbose than Warn, more verbose than Fatal
  | Error = 4
  /// The least verbose level. Will only pass through fatal
  /// log lines that cause the application to crash or become
  /// unusable.
  | Fatal = 5

/// When logging, write a log line like this with the source of your
/// log line as well as a message and an optional exception.
type LogLine =
  /// the source of the log line
  { path          : string
  /// the message that the application wants to log
  ; message       : string
  /// an optional exception
  ; ``exception`` : exn option }

/// The primary Logger abstraction that you can log data into
type Logger =
  /// log - evaluate the function if the log level matches
  abstract member Log : LogLevel -> (unit -> LogLine) -> unit

/// Log a line with the given format, printing the current time in UTC ISO-8601 format
/// and then the string, like such:
/// '2013-10-13T13:03:50.2950037Z: today is the day'
type ConsoleLogger(lvl) =
  interface Logger with
    member x.Log level f =
      if level >= lvl then
        let w = System.Console.WriteLine : string -> unit
        let line = f ()
        w(sprintf "%s: %s" (DateTime.UtcNow.ToString("o")) line.message)

// TODO: use abstraction above

let mutable enable_trace = false

let trace f_str =
  if enable_trace then
    System.Console.WriteLine(sprintf "%s: %s" (DateTime.UtcNow.ToString("o")) (f_str ()))

let tracef format =
  if enable_trace then
    format (Printf.kprintf (fun s -> System.Console.WriteLine(sprintf "%s: %s" (DateTime.UtcNow.ToString("o")) s)))

let log str =
  System.Console.WriteLine(sprintf "%s: %s" (DateTime.UtcNow.ToString("o")) str)

let logf format =
  Printf.kprintf
    (fun s -> System.Console.WriteLine(sprintf "%s: %s" (DateTime.UtcNow.ToString("o")) s))
    format
