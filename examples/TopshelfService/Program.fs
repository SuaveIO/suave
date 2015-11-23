open Suave
open Suave.Http.Successful
open Suave.Http
open Suave.AsyncOption.Operators
open Suave.Http.Applicatives
open Topshelf
open System
open System.Threading

[<EntryPoint>]
let main argv = 
    printfn "%A" argv


    let home = choose [path "/" >=> GET >=> OK "Hello world"]
    let mind = choose [path "/mind" >=> GET >=> OK "Where is my mind?"]
    let app = choose [ home; mind ]

    let cancellationTokenSource = ref None
    let start hc = 
        let cts = new CancellationTokenSource()
        let token = cts.Token
        let config = { defaultConfig with cancellationToken = token}
        startWebServerAsync config app
        |> snd
        |> Async.StartAsTask 
        |> ignore

        cancellationTokenSource := Some cts
        true

    let stop hc = 
        match !cancellationTokenSource with
        | Some cts -> cts.Cancel()
        | None -> ()
        true

    Service.Default 
    |> display_name "ServiceDisplayName"
    |> instance_name "ServiceName"
    |> with_start start
    |> with_stop stop
    |> run
