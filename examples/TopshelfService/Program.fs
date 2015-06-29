open Suave
open Suave.Http.Successful
open Suave.Web 
open Suave.Http
open Suave.Http.Applicatives
open Suave.Http.Successful
open Topshelf
open System
open System.Threading

[<EntryPoint>]
let main argv = 
    printfn "%A" argv

    let cancellationTokenSource = new CancellationTokenSource()
    let token = cancellationTokenSource.Token
    let config = { defaultConfig with cancellationToken = token}

    let home = choose [path "/" >>= GET >>= OK "Hello world"]
    let mind = choose [path "/mind" >>= GET >>= OK "Where is my mind?"]
    let app = choose [ home; mind ]

    let start hc = 
        startWebServerAsync config app
        |> snd
        |> Async.StartAsTask 
        |> ignore
        true

    let stop hc = 
        cancellationTokenSource.Cancel()
        true

    Service.Default 
    |> display_name "ServiceDisplayName"
    |> instance_name "ServiceName"
    |> with_start start
    |> with_stop stop
    |> with_topshelf
