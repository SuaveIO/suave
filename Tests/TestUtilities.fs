module Suave.Tests.TestUtilities

#nowarn "25"

open System
open System.IO
open System.Threading
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Reflection

open Suave
open Suave.Types
open Suave.Web
open Suave.Http
open Suave.Logging

open FsCheck

open Fuchu

type Arbs =
  static member String () = Arb.Default.String () |> Arb.filter (fun str -> str <> null)
let fscheck_config = { Config.Default with Arbitrary = [ typeof<Arbs> ] }

let current_path =
  Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)

let read_text relative_path =
  File.ReadAllText(Path.Combine(current_path, relative_path))

let read_bytes relative_path =
  File.ReadAllBytes(Path.Combine(current_path, relative_path))

let default_config = { default_config with logger = Loggers.sane_defaults_for LogLevel.Warn }
