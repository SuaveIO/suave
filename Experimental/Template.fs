/// A module for templating and generation of output
module Suave.Template

#nowarn "46"

open System
open System.Collections.Generic
open System.Xml
open System.IO
open System.Reflection
open System.Text

open Suave.Web
open Suave.Html
open Suave.Http

/// Load an object from its name
let load_object (str : string) =
  let entryAssembly = Assembly.GetEntryAssembly()
  entryAssembly.CreateInstance(str.Replace('.','+'))

/// Invoke the 'action' on the 'object' with 'args'
let invoke object action args =
  let typ = object.GetType()
  let meth = typ.GetMethod(action,[| typeof<Xml>|])
  meth.Invoke(object,[| args |])

/// Active Pattern that checks whether 'x' the string contains a pattern
/// and if so, returns the Pat( ... ) value containing the key-value pair
/// that was matched before and after ':'
let (|Pat|NoPat|) (x : String) =
  if x.Contains(":") then
    let index = x.IndexOf(':')
    Pat(x.Substring(0,index), x.Substring(index+1))
  else NoPat

/// Extract an XML/HTML template from the input 'xml', in 'ns1:keyhere' form
/// for tags that can be parsed. Replace those tags with their values as defined
/// in the 'bindings'.
let extract_template (nsx : string) (bindings : Map<string,Binder>) (xml : Xml) =
  let rec extract_template' (Xml(nodelist)) (Xml(result)) : Xml =
    let substitute = function
      | Element(Pat(ns, name), _, att), children
        when ns = nsx ->
        if bindings.ContainsKey(name) then
          let func = bindings.[name]
          let Xml(nodes) as f = func children
          f
        else failwith (sprintf "invalid binding name: '%s'\n" name)

      | Element(Pat("lift",name), _, att), children ->
        let obj = load_object name
        let action = Array.find (fun (x,_) -> x = "action") att |> snd
        invoke obj action children :?> Xml

      | elem, children ->
        Xml([elem,extract_template' children (Xml[])])

    let rec loop (nl:Node list) (Xml(acc) as res) =
      match nl with
      | []        -> res
      | p :: tail ->
        let Xml(subst) as s = substitute p
        loop tail (Xml(acc @ (subst)))

    loop nodelist (Xml([]))
  extract_template' (xml) (Xml([]))

/// Bind the matching elements, given the namespace 'ns', a XML/HTML document 'node'
/// and a 'bindings' map.
let bind (ns : string, node : Xml, bindings : Map<string,Binder>) =
  extract_template ns bindings node

/// An attribute array; an array of key-value pairs
type Attributes = (string*string)[]

/// Write the start of the tag 's' to the string writer
let write_begin_tag (s : string) n (a : Attributes) (stream : StringWriter) =
  let tt = Array.fold( fun z (x,y)  -> z + (sprintf "%s=\"%s\" " x y) ) " " a
  if tt = " " then stream.Write("<{0}>", s)
  else stream.Write("<{0} {1}>",s,tt)

/// Write the end of tag 's' to the string writer
let write_close_tag (s : string) n a (stream : StringWriter) =
  stream.Write("</{0}>", s)

/// Recursively convert the Xml structure to text into the string writer
let rec xml_to_string1 (Xml(nodes)) (stream : StringWriter) =
  for node in nodes do
    match node with
    | Element(s,n,a), children ->
      write_begin_tag s n a stream
      xml_to_string1 children stream
      write_close_tag s n a stream
    | Text(s), children ->
      stream.Write(s)
      xml_to_string1 children stream
    | WhiteSpace(s), children ->
      stream.Write(s)
      xml_to_string1 children stream

open Types

/// Process the template/html that is the requested LocalPath in the request Uri,
/// binding the 'user' elements to values from the 'data' bindings. If the processing
/// is successful, return the data as UTF-8 string in the response body; otherwise
/// write the exception as a string to a 500 Internal Error response.
let process_template (data : Map<string,Binder>) ({ request = http_request; runtime = runtime } as ctx : HttpContext) =
  try
    let xmlReader = new XmlTextReader(Files.resolve_path runtime.home_directory http_request.url)
    xmlReader.Namespaces <- false

    let transform = parser xmlReader (Xml [])

    let xml = extract_template "user" data transform
    let sb = new StringBuilder()
    let str = new StringWriter(sb)
    xml_to_string1 xml str
    let output = sb.ToString()
    Successful.OK output ctx
  with
  | x -> ServerErrors.INTERNAL_ERROR (x.ToString()) ctx
