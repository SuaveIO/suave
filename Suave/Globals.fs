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

/// the logging configuration is global, or else we need to extend all function
/// with a parameter holding the logger, transitively upwards towards the point
/// of program composition. While this is an option, logging is intended no to
/// have side-effects, so making it globally configured is a fair trade-off
/// against complexity in the rest of suave.
let internal logging_config : obj option ref = ref None