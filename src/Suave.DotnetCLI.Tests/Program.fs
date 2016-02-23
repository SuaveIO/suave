open System
open Suave

[<EntryPoint>]
let main argv = 

    startWebServer defaultConfig (Successful.OK "Hello World!")

    0
