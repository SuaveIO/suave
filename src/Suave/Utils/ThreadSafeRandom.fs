module Suave.Utils.ThreadSafeRandom

open System
open System.Threading

let private seed = ref Environment.TickCount
let private rnd = new ThreadLocal<Random>(fun () -> Random (Interlocked.Increment seed))

/// fill buffer with random bytes
let nextBytes (buffer : byte []) =
  rnd.Value.NextBytes buffer

/// generate a new random int32 value bounded to [minInclusive; maxExclusive)
let next (minInclusive : int) (maxExclusive : int) =
  rnd.Value.Next(minInclusive, maxExclusive)

/// generate a new random ulong64 value
let nextUInt64 () =
  let buffer = Array.zeroCreate<byte> sizeof<UInt64>
  rnd.Value.NextBytes buffer
  BitConverter.ToUInt64(buffer, 0)
