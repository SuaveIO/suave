module Suave.Tests.Http2

open Fuchu

open System
open System.IO
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Threading

open Suave
open Suave.Operators
open Suave.Sockets
open Suave.Sockets.Control
open Suave.Http2
open Suave.Utils
open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let frameHeaders (_ : SuaveConfig) =
  testList "frame headers" [
    testCase "smoke" 
    <| fun _ ->
       let originalHeader = { length = 500u; ``type`` = 2uy; flags = 1uy; streamIdentifier = 16777215u }
       let encoded = encodeFrameHeader originalHeader
       let header = parseFrameHeader encoded
       Assert.Equal("encode >> decode == id", originalHeader, header)
  ]