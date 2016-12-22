// Learn more about F# at http://fsharp.net
// See the 'F# Tutorial' project for more help.
namespace SignalRDemo

module Hubs =
    open Microsoft.AspNet.SignalR
    
    type ISendPings =
        abstract SendPing : string -> unit
    type DemoHub () =
        inherit Hub<ISendPings>()
        member this.SendLogPing = this.Clients.All.SendPing
        
        
module Main =
    open Owin
    open Microsoft.Owin
    open Microsoft.Owin.Builder
    
    open Suave
    open Suave.Owin
    open Suave.Http
    open Suave.Operators
    open Suave.Filters
    
    open System
    open System.Threading
    open Hubs
    let startSendingNonsense () = 
        let rec thing (i : int)  = async {
            do! Async.Sleep(1000)
            let hubContext = Microsoft.AspNet.SignalR.GlobalHost.ConnectionManager.GetHubContext<DemoHub, ISendPings> ()
            i |> string |> hubContext.Clients.All.SendPing 
            return! thing(i + 1)
        }
        thing 0 |> Async.Start
    
    [<EntryPoint>]
    let main argv = 
        
        
        let builder = new AppBuilder()
        let builder = builder.MapSignalR()
        let owin = builder.Build() |> OwinApp.ofAppFunc ""
        let app = 
            
            choose [
                GET >=> path "/" >=> Files.browseFileHome "index.html"
                owin
            ]
        
        let cts = new CancellationTokenSource()
        let suaveConfig =  defaultConfig
        let lisening, server = 
            startWebServerAsync suaveConfig app
        Async.Start(server, cts.Token)
        startSendingNonsense()
        Console.ReadLine() |> ignore
        cts.Cancel()
        0 // return an integer exit code
    
