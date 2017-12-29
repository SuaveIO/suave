module Suave.Globals

open System

/// Get the current DateTimeOffset in UTC format.
///
/// If you are unit-testing
/// you can set this, but as with all globals, you need to set it back afterwards
/// or you will break subsequent unit tests.
let mutable utcNow = fun () -> System.DateTimeOffset.UtcNow

/// From the TCP module, keeps track of the number of clients
let internal numberOfClients = ref 0L

open System.Collections.Concurrent

/// A map of compressed files. The key is the is the name of the file, and value is 
/// a pair: the name of the compressed file and timestamp of the original file at 
/// time of compression.
let internal compressedFilesMap = new ConcurrentDictionary<string,string * DateTime>()

open System
open System.Reflection

/// This returns the assembly version of Suave
let SuaveVersion = Assembly.GetExecutingAssembly().GetName().Version.ToString()

/// This is the server header
let ServerHeader = String.Concat [| "Server: Suave (https://suave.io)" |]

[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.LibUv")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Experimental")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.DotLiquid")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Razor")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Xsp")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Tests")>]
do ()