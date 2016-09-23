namespace Suave.NancyFx

open Suave
open Suave.Owin
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

  [<EntryPoint>]
  let main argv =
    let opts = new NancyOptions()
    let app = OwinApp.ofMidFunc "/" (NancyMiddleware.UseNancy(opts))  =>= RequestErrors.NOT_FOUND "File not found"
    startWebServer defaultConfig app
    0