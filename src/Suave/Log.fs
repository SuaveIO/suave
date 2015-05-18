namespace Suave

[<CompilationRepresentation(CompilationRepresentationFlags.ModuleSuffix)>]
module Log =

  open System
  open System.Diagnostics
  
  open Suave.Logging
  open Suave.Utils
  open Suave.Utils.RandomExtensions

  let verbose (logger : Logger) path trace message =
    logger.Log LogLevel.Verbose (fun _ -> LogLine.mk path LogLevel.Verbose trace None message)

  let verbosef logger path trace f_format =
    f_format (Printf.kprintf (verbose logger path trace))

  let verbosee (logger : Logger) path trace ex message =
    logger.Log LogLevel.Verbose (fun _ -> LogLine.mk path LogLevel.Verbose trace (Some ex) message)

  let info (logger : Logger) path trace message =
    logger.Log LogLevel.Info (fun _ -> LogLine.mk path LogLevel.Info trace None message)

  let infof logger path trace f_format =
    f_format (Printf.kprintf (info logger path trace))

  let infoe (logger : Logger) path trace ex message =
    logger.Log LogLevel.Info (fun _ -> LogLine.mk path LogLevel.Info trace (Some ex) message)

  let intern (logger : Logger) path =
    verbose logger path (TraceHeader.empty)

  let interne (logger : Logger) path =
    verbosee logger path (TraceHeader.empty)

  let internf (logger : Logger) path f_format =
    f_format (Printf.kprintf (verbose logger path (TraceHeader.empty)))

  let log (logger : Logger) path level msg =
    LogLine.mk path level TraceHeader.empty None msg
    |> fun line -> logger.Log level (fun () -> line)