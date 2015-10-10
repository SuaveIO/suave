namespace Suave.Examples.WebSharper

module SampleSite =
  open WebSharper
  open WebSharper.Sitelets
  open WebSharper.Html.Server

  type EndPoint =
    | Index

  let IndexContent context : Async<Content<EndPoint>> =
    let time = System.DateTime.Now.ToString()
    Content.Page(
      Title = "Index",
      Body = [H1 [Text ("Current time: " + time)]]
    )

  [<Website>]
  let MySampleWebsite : Sitelet<EndPoint> =
    Sitelet.Content "/index" EndPoint.Index IndexContent

module SelfHostedServer =

  open global.Owin
  open System.Net
  open System
  open System.Diagnostics
  open Suave
  open Suave.Types
  open Suave.Http.Successful
  open Suave.Web
  open Suave.Logging
  open Suave.Owin
  open WebSharper.Owin

  [<EntryPoint>]
  let Main argv =

    let app =
      let root = ".."
      SiteletMiddleware<_>.AsMidFunc(Options.Create(root), SampleSite.MySampleWebsite)
      |> Suave.Owin.OwinApp.ofMidFunc "/"

    let config =
      { defaultConfig with
          logger = Loggers.saneDefaultsFor LogLevel.Verbose }

    startWebServer config app
    0
