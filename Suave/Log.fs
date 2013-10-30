module Suave.Log

open System
open System.Diagnostics

let private ic = System.Globalization.CultureInfo.InvariantCulture
let private sync_root = obj()
/// Log a line with the given format, printing the current time in UTC ISO-8601 format
/// and then the string, like such:
/// '2013-10-13T13:03:50.2950037Z: today is the day'
let log format =
  Printf.kprintf (lock sync_root <| fun () -> printfn "%s: %s" (DateTime.UtcNow.ToString("o", ic))) format
