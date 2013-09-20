
module Suave.Tests

open System
open System.Threading
open System.Net.Http

open NUnit.Framework
open FsUnit

open Suave.Web
open Suave.Http


[<Test>] 
let ``hello world`` () = 
    
    let server = web_server_async defaultConfig (OK "Hello World!")
    Async.Start(server)
    
    //give the server a chance to start
    //Thread.Sleep(1000)
    
    let client = new HttpClient()
    
    let response = client.GetAsync(sprintf "http://127.0.0.1:8083%s" "/hello") |> Async.AwaitTask
    
    let message = Async.RunSynchronously response
    message.EnsureSuccessStatusCode() |> ignore
    
    let content = message.Content.ReadAsStringAsync() |> Async.AwaitTask
    
    let str = Async.RunSynchronously content
    
    Assert.AreEqual(str,"Hello World!")
    
    Async.CancelDefaultToken()