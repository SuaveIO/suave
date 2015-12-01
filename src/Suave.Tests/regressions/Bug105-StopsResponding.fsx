#r "../bin/Release/Suave.dll"
#r "System.Net"
#r "System.Net.Http"
#r "System.Xml"
#r "System.Xml.Linq"

module XD =
  open System.IO
  open System.Xml
  open System.Xml.Linq

  let mkXDoc raw_bytes =
    use xml_stream = new MemoryStream(raw_bytes : byte [])
    use rdr = new XmlTextReader(xml_stream)
    XDocument.Load rdr

module SystemUnderTest =
  open Suave
  open Suave.Http
  open Suave.Http.Successful
  open Suave.Http.RequestErrors
  open Suave.Http.ServerErrors
  open Suave.Http.Applicatives
  open Suave.Types
  open Suave.Web
  open Suave.Log
  open Suave.Logging

  let app =
    choose [
      path "/deserialiseXml"
        >>= request (fun r -> r.rawForm |> XD.mkXDoc |> ignore
                              ACCEPTED ".")
      NOT_FOUND "bad route" ]

  let conf =
    { Web.defaultConfig with
        logger = Loggers.saneDefaultsFor LogLevel.Verbose
        errorHandler =
          (fun ex reason ->
            printfn "error: %O" ex
            INTERNAL_ERROR "Failed test" ) }

  let run () =
    let ready, listens = startWebServerAsync conf app
    listens |> Async.Start
    ready |> Async.RunSynchronously |> ignore

module Client =
  open System
  open System.IO
  open System.Net.Http
  open System.Threading

  let clientPost (uri : Uri) (ct : CancellationToken) (client : HttpClient) (data : 'a) =
    let r = new HttpRequestMessage(HttpMethod.Post, uri)
    r.Headers.ConnectionClose <- Nullable(true)
    client.PostAsync(uri, data, ct) |> Async.AwaitTask

  let uri str =
    Uri str

  let withClient f =
    use cts = new CancellationTokenSource()
    use handler = new Net.Http.HttpClientHandler(AllowAutoRedirect = true)
    use client = new Net.Http.HttpClient(handler)
    f cts handler client

  let posts = ref 0

  let post (data : byte[]) =
    printfn "#%d|%d" (Interlocked.Increment(posts)) (data.Length)
    withClient <| fun cts handler client ->
      new ByteArrayContent(data)
      |> clientPost (uri "http://localhost:9001/deserialiseXml") (cts.Token) client
      |> Async.RunSynchronously

  let postAndAssert =
    post
    >> fun (m : HttpResponseMessage) ->
          let res = (m.Content.ReadAsStringAsync() |> Async.AwaitTask |> Async.RunSynchronously)
          m.EnsureSuccessStatusCode() |> ignore
          printf "%s" res

  let ad = Path.Combine(__SOURCE_DIRECTORY__, "large_xml.xml") |> File.ReadAllBytes

  let gen len =
    String.Concat [| """<?xml version="1.0" encoding="utf-8"?>
<data>"""; (String.replicate len "#"); "</data>" |]
    |> Suave.Utils.UTF8.bytes

  let run rnd =
    let seed = DateTime.UtcNow.Ticks |> int
    let r = rnd |> Suave.Utils.Option.orDefault (Random seed)
    printfn "client running with seed: %d" seed
    for i in 1 .. 10 * (SystemUnderTest.conf.maxOps - 1) do
      try
        // from 0 to 20 MiB sent
        postAndAssert (gen (1 * r.Next(2. ** 20. |> int)))
      with :? System.AggregateException as e when e.ToString().Contains("The underlying connection was closed") ->
        printfn "%O" e

// run test
SystemUnderTest.run ()
Client.run None
// System.GC.Collect()
// wait for all fsi evaluations to complete
System.Threading.Thread.Sleep 5000

// now if the test stopped, try:
Client.postAndAssert Client.ad

//Tcp.tcpIpServer.job