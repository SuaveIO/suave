namespace Suave

module IdentityServer =

  open System.Collections.Generic
  open Owin
  open Microsoft.Owin.Builder
  open Suave
  open Suave.Logging
  open Suave.Logging.Message
  open Suave.Owin
  open Suave.Filters
  open Suave.Operators
  open Suave.Successful
  open IdentityServer3.Core.Configuration
  open IdentityServer3.Core.Models
  open IdentityServer3.Core.Services.InMemory
  open System.Security.Cryptography.X509Certificates
  open System.Net
  open Suave.Logging
  open System
  open System.Threading

  let certificate = new X509Certificate2("IdentityServerExample.pfx")

  /// A WebPart that applies security
  let securityMiddleware =

    let builder = new AppBuilder()

    let scopes = [
      new Scope(Name = "devices")]

    let clients = [
      new Client(
        ClientName = "Smartphone App",
        ClientId="smartphone",
        Enabled=true,
        AccessTokenType=AccessTokenType.Jwt,
        Flow = Flows.ClientCredentials,
        ClientSecrets = new List<Secret>([ new Secret("89C4C260-08B5-4B8F-82A7-7B513B8CB8BA".Sha256())]),
        AllowedScopes = new List<string>(scopes |> Seq.map (fun v -> v.Name)))]
    let users =
      new List<InMemoryUser>( seq{
        yield new InMemoryUser(Username = "bob", Password = "secret", Subject = "1")
        yield new InMemoryUser(Username = "alice", Password = "secret", Subject = "2")})

    let serviceFactory =
      (new IdentityServerServiceFactory())
        .UseInMemoryScopes(scopes)
        .UseInMemoryClients(clients)
        .UseInMemoryUsers(users)
    let options =
      new IdentityServerOptions(
        SiteName = "AtwoodHome",
        Factory = serviceFactory,
        SigningCertificate = certificate,
        RequireSsl = false,
        EnableWelcomePage = true)
    let builder = builder.UseIdentityServer(options)
    let owinApp = builder.Build()

    OwinApp.ofAppFunc "/identity" owinApp

  let webApp =
    choose [
      path "/hello" >=> OK "Hello!"
      securityMiddleware
    ]

  [<EntryPoint>]
  let main _ =
    let logger = Targets.create Verbose

    let loggedWebApp context = async {
      logger.log Debug (
        eventX "Received request {method} {url}"
        >> setField "method" context.request.``method``
        >> setField "url" context.request.url)

      let! response = webApp context
      return response }

    let bindings =
      let host = Dns.GetHostEntry(Dns.GetHostName())
      HttpBinding.defaults :: [
        for ip in host.AddressList do
          yield HttpBinding.create Protocol.HTTP ip HttpBinding.DefaultBindingPort
          yield HttpBinding.create (Protocol.HTTPS certificate) ip 8443us]

    let config = { defaultConfig with bindings = bindings; logger = logger }
    let _, server = startWebServerAsync config loggedWebApp
    let cancellationTokenSource = new CancellationTokenSource()
    Async.Start( server, cancellationTokenSource.Token )

    Console.ReadKey() |> ignore

    cancellationTokenSource.Cancel()

    0
