module Suave.Log

open System
open System.Diagnostics

/// Log a line with the given format, printing the current time in UTC ISO-8601 format
/// and then the string, like such:
/// '2013-10-13T13:03:50.2950037Z: today is the day'
let log format =
  Printf.kprintf (fun s -> System.Console.WriteLine(sprintf "%s: %s" (DateTime.UtcNow.ToString("o")) s)) format
