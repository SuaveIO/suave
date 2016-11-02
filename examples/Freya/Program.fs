namespace Suave.Examples.Freya

module SampleApp =
    open System.Text
    open Freya.Core
    open Freya.Machines.Http
    open Freya.Routers.Uri.Template

    let name =
        freya {
            let! name = Freya.Optic.get (Route.atom_ "name")

            match name with
            | Some name -> return name
            | _ -> return "World" }

    let hello =
        freya {
            let! name = name

            return Represent.text (sprintf "Hello %s!" name) }

    let machine =
        freyaMachine {
            handleOk hello }

    let router =
        freyaRouter {
            resource "/hello{/name}" machine }

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
      OwinApp.ofAppFunc "/" (OwinAppFunc.ofFreya router)

    let config =
      { defaultConfig with
          bindings = [ HttpBinding.createSimple HTTP "127.0.0.1" 7000 ]
          logger = Targets.create Verbose }

    printfn "Listening on port 7000"
    startWebServer config app

    0
