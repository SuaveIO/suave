module Suave.Globals

/// the global random pool for quick random values, e.g. for generating
/// random large numbers to identify requests
let random = System.Random()

/// the global crypto-random pool for uniform and therefore cryptographically
/// secure random values
let crypt_random = System.Security.Cryptography.RandomNumberGenerator.Create()

// Note: these are global, because they can be (System.Random is) time-dependent
// and therefore, "However, because the clock has finite resolution, using the
// parameterless constructor to create different Random objects in close succession
// creates random number generators that produce identical sequences of random numbers."
// - MSDN.

/// Get the current DateTimeOffset in UTC format.
let mutable utc_now = fun () -> System.DateTimeOffset.UtcNow

/// From the TCP module, keeps track of the number of clients
let internal number_of_clients = ref 0L


// TODO: provide proper session storage
open System.Collections.Concurrent

/// Static dictionary of sessions - here be dragons, see Session.fs
let internal session_map = new ConcurrentDictionary<string, ConcurrentDictionary<string, obj>>()

/// A map of compressed files:
/// TODO - evaluate if we can't service requests
/// by writing these to disk instead
let internal compressed_files_map = new ConcurrentDictionary<string,string>()
