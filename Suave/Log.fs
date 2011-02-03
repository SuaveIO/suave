module Suave.Log

open System
open System.Diagnostics

let console (s:string) =  Console.Write(s);

let log format = 
    let callr = (new StackFrame(1)).GetMethod().Name 
    (Printf.kprintf (printf "%A: %s - %s" DateTime.Now callr) format) 