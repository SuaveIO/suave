#r "bin/Release/suave.dll"
#r "System.Net"
#r "System.Net.Http"
#r "System.Xml"
#r "System.Xml.Linq"

module XD =
  open System.IO
  open System.Xml
  open System.Xml.Linq
  let mk_xdoc raw_bytes =
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

  let app =
    choose [
      url "/deserialise_xml"
        >>= request (fun r -> r.raw_form |> XD.mk_xdoc |> ignore
                              ACCEPTED ".")
      NOT_FOUND "bad route" ]

  let conf =
    { Web.default_config with
        logger = Loggers.sane_defaults_for Verbose
        error_handler =
          (fun ex reason ->
            printfn "error: %O" ex
            INTERNAL_ERROR "Failed test" >> (fun opt -> opt |> Option.get)) }

  let run () =
    let ready, listens =
      Web.web_server_async conf app
    listens |> Async.Start
    ready |> Async.RunSynchronously |> ignore

module Client =
  open System
  open System.IO
  open System.Net.Http
  open System.Threading

  let client_post (uri : Uri) (ct : CancellationToken) (client : HttpClient) (data : 'a) =
    let r = new HttpRequestMessage(HttpMethod.Post, uri)
    r.Headers.ConnectionClose <- Nullable(true)
    client.PostAsync(uri, data, ct) |> Async.AwaitTask

  let uri str =
    Uri str

  let with_client f =
    use cts = new CancellationTokenSource()
    use handler = new Net.Http.HttpClientHandler(AllowAutoRedirect = true)
    use client = new Net.Http.HttpClient(handler)
    f cts handler client

  let posts = ref 0

  let post data =
    printfn "#%d" <| Interlocked.Increment(posts)
    with_client <| fun cts handler client ->
      new ByteArrayContent(data)
      |> client_post (uri "http://localhost:8083/deserialise_xml") (cts.Token) client
      |> Async.RunSynchronously

  let post_and_assert =
    post
    >> fun (m : HttpResponseMessage) ->
          let res = (m.Content.ReadAsStringAsync() |> Async.AwaitTask |> Async.RunSynchronously)
          m.EnsureSuccessStatusCode() |> ignore
          printf "%s" res

  let ad = Path.Combine(__SOURCE_DIRECTORY__, "large_xml.xml") |> File.ReadAllBytes

  let run () =
    for i in 1 .. (SystemUnderTest.conf.max_ops - 1) do
      if i % 1000 = 0 then printfn "at %i" i
      try
        post_and_assert ad
      with :? System.AggregateException as e when e.ToString().Contains("The underlying connection was closed") ->
        printfn "%O" e

// run test
SystemUnderTest.run ()
Client.run()

// wait for all fsi evaluations to complete
System.Threading.Thread.Sleep 5000

// now if the test stopped, try:
Client.post_and_assert Client.ad