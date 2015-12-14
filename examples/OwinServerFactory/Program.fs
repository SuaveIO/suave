﻿namespace OwinServerFactory

module NancyModules =
    open Nancy
    type App() as x =
      inherit NancyModule()
      do
        x.Get.["/"] <- fun _ -> "Hello World" :> obj

module OwinStartup =
    open Owin   
    open Nancy.Owin
    type Startup () =
        member __.Configuration(app : IAppBuilder) =
            app.UseNancy() 
            |> ignore

module Main =
    open System
    open Microsoft.Owin.Hosting
    [<EntryPoint>]
    let main argv = 
        try
            let port = 8080
            let options = StartOptions ()
            options.Port <- port |> Nullable<int>
            options.ServerFactory <- "Suave.Owin+OwinServerFactory"
            use app = WebApp.Start<OwinStartup.Startup>(options)
            printfn "Server started on port %d" port
            Console.ReadLine() |> ignore
        with ex -> printfn "%A" ex
        Console.ReadLine() |> ignore
        0 // return an integer exit code

