namespace Suave.LibUv

open Suave
open Suave.LibUv.Tcp

type LibUvServerFactory() =
  interface TcpServerFactory with
    member this.create (logger, maxOps, bufferSize, binding) =
      runServerLibUv logger maxOps bufferSize binding