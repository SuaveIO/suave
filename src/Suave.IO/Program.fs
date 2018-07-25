module Suave.IO.Program

open Argu
open Suave
open Suave.Filters
open Suave.Redirection
open Suave.RequestErrors
open Suave.ServerErrors
open Suave.Operators
open Suave.Model
open Suave.Logging
open Suave.Logging.Message
open System.IO
open System.Net
open Stripe

let logger = Log.create "Suave.IO"

type Arguments =
  | [<Mandatory>] Binding of string * int
  | [<Mandatory>] Home of string
with
  interface IArgParserTemplate with
    member s.Usage =
      match s with
      | Home _ -> "specify a working directory."
      | Binding _ -> "specify a listener '<hostname> <port>'"

let addExnLogging (fwp: 'a -> WebPart) =
  fun input ctx ->
    async {
      try
        return! fwp input ctx
      with e ->
        do logger.fatal (eventX "Unhandled {exception}" >> setField "exception" e)
        return! INTERNAL_ERROR "Unhandled internal exception" ctx
    }

let subscribeCustomer =
  let callApi (email, token, tokenType, planId: string): WebPart =
    let custs = StripeCustomerService()
    let subs = StripeSubscriptionService()
    fun ctx ->
      async {
        let opts = StripeCustomerCreateOptions()
        opts.SourceToken <- token
        opts.Email <- email
        opts.Description <- "From Suave.io site"
        opts.PlanId <- planId

        let! cust = custs.CreateAsync(opts)
        let! sub = subs.CreateAsync((cust :> StripeEntityWithId).Id, planId)
        return! FOUND "promote-thank-you.html" ctx
      }

  bindReq (fun r -> binding {
      let! email = r.formData "stripeEmail"
      let! token = r.formData "stripeToken"
      let! tokenType = r.formData "stripeToken"
      let! planId = r.formData "stripePlanId"
      return email, token, tokenType, planId
  }) (addExnLogging callApi) BAD_REQUEST

let app: WebPart =
  choose [
    POST >=> path "/promote" >=> subscribeCustomer
    Files.browseHome
    Files.browseFileHome "index.html"
    request (fun r -> INTERNAL_ERROR (sprintf "No file found at path %s" r.url.AbsolutePath))
  ]

[<EntryPoint>]
let main argv =
  let parser = ArgumentParser.Create("""dotnet run -- --binding <ip4oripv6> <port> --home <homedir>""")
  let parsed = parser.Parse argv
  let (host, port) = parsed.GetResult <@ Binding @>
  let home = parsed.GetResult <@ Home @> |> Path.GetFullPath
  ServicePointManager.SecurityProtocol <- SecurityProtocolType.Tls12
  StripeConfiguration.SetApiKey (Env.varRequired "STRIPE_PRIVATE_KEY")

  printfn "Root path: %s" home

  let config =
    { defaultConfig with
        bindings = [ HttpBinding.createSimple HTTP host port ]
        homeFolder = Some home }

  startWebServer config app
  0