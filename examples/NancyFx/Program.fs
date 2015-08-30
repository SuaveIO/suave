namespace Suave.NancyFx

open Suave
open Suave.Types
open Suave.Http
open Suave.Web
open Suave.Owin
open System.Threading.Tasks
open Nancy
open Nancy.Owin

type App() as x =
  inherit NancyModule()
  do
    x.Get.["/"] <- fun _ -> "Hello World, from NancyFx on Suave!" :> obj
    x.Get.["/fsharp"] <- fun _ -> "I can into F#" :> obj
    x.Get.["/json"] <- fun _ -> x.Response.AsJson([ "Test" ]) :> obj
    x.Get.["/complex"] <- fun _ -> 
      let response = x.Response.AsJson(["This is my Response"])
      response.ContentType <- "application/json"
      response.Headers.Add("Funky-Header", "Funky-Header-Value")
      response :> obj

module Program =
  let utterMagicWordsOfCode (opts : NancyOptions) : WebPart =
    let appFunc = NancyMiddleware.UseNancy(opts).Invoke(fun _ -> new Task(fun _ -> ()))
    OwinAppFunc.ofOwinFunc appFunc

  [<EntryPoint>]
  let main argv =
    let opts = new NancyOptions()
    let app = utterMagicWordsOfCode opts
    startWebServer defaultConfig app
    0