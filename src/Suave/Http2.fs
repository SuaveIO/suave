namespace Suave

/// <summary><para>
/// The HTTP module has these main sub-modules:
/// </para>
/// <list>
///   <item>Response - response and response_f functions.</item>
///   <item>Writers - ways to modify the response.</item>
///   <item>Intermediate - 100 and 101 response codes.</item>
///   <item>Successful- 2xx response codes.</item>
///   <item>Redirection - 3xx response codes.</item>
///   <item>RequestErrors - 4xx response codes.</item>
///   <item>ServerErrors - 5xx response codes.</item>
///   <item>Applicatives - use to filter down the request to something you want</item>
///   <item>Files - send files to the client</item>
///   <item>Authentication - Methods for authenticating http requests</item>
/// </list>
/// <para>and these main types:</para>
/// <list>
///   <item>WebResult = Async&lt;unit&gt; option</item>
///   <item>WebPart = HttpContext -&gt; WebResult</item>
///   <item>HttpMethod</item>
/// </list>
/// </summary>
module Http2 =
  open Suave.Sockets
  open Suave.Types

  
  let inline suceed x = Http.succeed x
  let fail = Http.fail
  let never = Http.never
  let choose = Http.choose 
  let inline warbler f a = Http.warbler f a
  let inline cnst x = Http.cnst x
  let cond = Http.cond

  let (>=>) first second = Http.bind second first

  let (<=<) first second = Http.bind first second

  [<AutoOpen>]
  module AsyncExtension =
    let (>>=) (vl:Async<'T>) (fn : 'T -> Async<'Q>) =
      async {
        let! e = vl
        return! fn e}

  [<AutoOpen>]
  module OptionExtension =
    let (>>=) (vl:'T Option) (fn : 'T -> 'Q Option) =
      match vl with
        | Some(v) -> fn v
        | None -> None
  
  module Response = Http.Response
  module Intermediate = Http.Intermediate
  module Successful = Http.Successful
  module Redirection = Http.Redirection
  module RequestErrors = Http.RequestErrors
  module ServerErrors = Http.ServerErrors
  module Applicatives = Http.Applicatives
  module Files = Http.Files
  module Authentication = Http.Authentication
