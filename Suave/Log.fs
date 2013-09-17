module Suave.Log

open System
open System.Diagnostics

let console (s:string) =  Console.Write(s);

let log format = 
    (Printf.kprintf (printf "%A: %s" DateTime.Now) format) 