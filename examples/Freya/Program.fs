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

    let en = LanguageTag.Parse "en"

    let inline represent (x : string) =
        { Description =
            { Charset = Some Charset.Utf8
              Encodings = None
              MediaType = Some MediaType.Text
              Languages = Some [ en ] }
          Data = Encoding.UTF8.GetBytes x }

    let ok = freya {
        do! Freya.Optic.set Response.reasonPhrase_ (Some "Hey Folks!")
        let! stream = Freya.Optic.get Response.body_
        let out = "Hello, folks!"B
        stream.Write(out, 0, out.Length)
    }
         //*> Freya.init (represent "Hey, folks!")

//    let common =
//        freyaMachine {
//            using http
//            charsetsSupported Charset.Utf8
//            languagesSupported en
//            mediaTypesSupported MediaType.Text }
//
//    let home =
//        freyaMachine {
//            using http
//            //including common
//            methodsSupported GET
//            handleOk ok }

    let routes =
        freyaRouter {
            resource "/" ok }

module SelfHostedServer =
    open Freya.Core
    open Suave.Logging
    open Suave.Owin
    open Suave.Types
    open Suave.Web
    open SampleApp

    [<EntryPoint>]
    let main _ =

        let app =
            OwinApp.ofAppFunc "/" (OwinAppFunc.ofFreya routes)

        let config =
            { defaultConfig with
                bindings = [ HttpBinding.mkSimple HTTP "127.0.0.1" 7000 ]
                logger = Loggers.saneDefaultsFor LogLevel.Verbose }

        printfn "Listening on port 7000"
        startWebServer config app

        0
