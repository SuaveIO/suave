namespace Suave.LibUv

open Suave
open Suave.LibUv.Tcp

type LibUvServerFactory() =
  interface TcpServerFactory with
    member this.create (logger, maxOps, bufferSize, autoGrow, binding) =
      runServerLibUv logger maxOps bufferSize autoGrow binding