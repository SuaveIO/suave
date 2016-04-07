module Suave.Globals

/// Get the current DateTimeOffset in UTC format.
///
/// If you are unit-testing
/// you can set this, but as with all globals, you need to set it back afterwards
/// or you will break subsequent unit tests.
let mutable utcNow = fun () -> System.DateTimeOffset.UtcNow

/// From the TCP module, keeps track of the number of clients
let internal numberOfClients = ref 0L

open System.Collections.Concurrent

/// A map of compressed files:
/// TODO - evaluate if we can't service requests
/// by writing these to disk instead
let internal compressedFilesMap = new ConcurrentDictionary<string,string>()

open System
open System.Reflection

/// This returns the assembly version of Suave
#if NETSTANDARD1_5
type private ThisGlobal = { UsedByGetExecutingAssembly: string }

let SuaveVersion = typeof<ThisGlobal>.GetTypeInfo().Assembly.GetName().Version.ToString()
#else
let SuaveVersion = Assembly.GetExecutingAssembly().GetName().Version.ToString()
#endif

/// This is the server header
let ServerHeader = String.Concat [| "Server: Suave (https://suave.io)" |]

[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Experimental")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.DotLiquid")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Razor")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Xsp")>]
[<assembly:System.Runtime.CompilerServices.InternalsVisibleToAttribute("Suave.Tests")>]
do ()