namespace Suave.LibUv

open Suave
open Suave.LibUv.Tcp

type LibUvServerFactory() =
  interface TcpServerFactory with
    member this.create (maxOps, bufferSize, autoGrow, binding) =
      runServerLibUv maxOps bufferSize autoGrow binding