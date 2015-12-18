namespace Suave.Examples.Freya

module SampleApp =
    open System.Text
    open Arachne.Language
    open Arachne.Http
    open Freya.Core
    open Freya.Core.Operators
    open Freya.Lenses.Http
    open Freya.Machine
    open Freya.Machine.Extensions.Http
    open Freya.Machine.Router
    open Freya.Router
    open Arachne.Uri.Template

    let en = LanguageTag.Parse "en"

    let inline represent (x : string) =
        { Description =
            { Charset = Some Charset.Utf8
              Encodings = None
              MediaType = Some MediaType.Text
              Languages = Some [ en ] }
          Data = Encoding.UTF8.GetBytes x }

    let ok =
            Freya.Lens.setPartial Response.ReasonPhrase_ "Hey Folks!"
         *> Freya.init (represent "Hey, folks!")

    let common =
        freyaMachine {
            using http
            charsetsSupported (Freya.init [ Charset.Utf8 ])
            languagesSupported (Freya.init [ en ])
            mediaTypesSupported (Freya.init [ MediaType.Text ]) }

    let home =
        freyaMachine {
            using http
            //including common
            methodsSupported (Freya.init [ GET ])
            handleOk (fun _ -> ok) }

    let routes =
        freyaRouter {
            resource (UriTemplate.Parse "/") (home |> FreyaMachine.toPipeline) }
        |> FreyaRouter.toPipeline

module SelfHostedServer =
  open Freya.Core
  open Suave
  open Suave.Logging
  open Suave.Owin
  open SampleApp

  [<EntryPoint>]
  let main _ =

    printfn "w00t"

    let app =
      OwinApp.ofAppFunc "/" (OwinAppFunc.ofFreya routes)

    let config =
      { defaultConfig with
          bindings = [ HttpBinding.mkSimple HTTP "127.0.0.1" 7000 ]
          logger = Loggers.saneDefaultsFor LogLevel.Verbose }

    printfn "Listening on port 7000"
    startWebServer config app

    0
