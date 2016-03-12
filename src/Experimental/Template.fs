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
open Suave.Xml
open Suave.Operators

/// Load an object from its name
let loadObject (str : string) =
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

/// A binder takes XML, applies a transform, and returns the XML back.
type Binder = Xml -> Xml

/// Extract an XML/HTML template from the input 'xml', in 'ns1:keyhere' form
/// for tags that can be parsed. Replace those tags with their values as defined
/// in the 'bindings'.
let extractTemplate (nsx : string) (bindings : Map<string,Binder>) (xml : Xml) =
  let rec extractTemplate' (Xml(nodelist)) (Xml(result)) : Xml =
    let substitute = function
      | Element(Pat(ns, name), _, att), children
        when ns = nsx ->
        if bindings.ContainsKey(name) then
          let func = bindings.[name]
          let Xml(nodes) as f = func children
          f
        else failwith (sprintf "invalid binding name: '%s'\n" name)

      | Element(Pat("lift",name), _, att), children ->
        let obj = loadObject name
        let action = Array.find (fun (x,_) -> x = "action") att |> snd
        invoke obj action children :?> Xml

      | elem, children ->
        Xml([elem,extractTemplate' children (Xml[])])

    let rec loop (nl:Node list) (Xml(acc) as res) =
      match nl with
      | []        -> res
      | p :: tail ->
        let Xml(subst) as s = substitute p
        loop tail (Xml(acc @ (subst)))

    loop nodelist (Xml([]))
  extractTemplate' (xml) (Xml([]))

/// Bind the matching elements, given the namespace 'ns', a XML/HTML document 'node'
/// and a 'bindings' map.
let bind (ns : string, node : Xml, bindings : Map<string,Binder>) =
  extractTemplate ns bindings node

/// An attribute array; an array of key-value pairs
type Attributes = (string*string)[]

/// Write the start of the tag 's' to the string writer
let writeBeginTag (s : string) n (a : Attributes) (stream : StringWriter) =
  let tt = Array.fold( fun z (x,y)  -> z + (sprintf "%s=\"%s\" " x y) ) " " a
  if tt = " " then stream.Write("<{0}>", s)
  else stream.Write("<{0} {1}>",s,tt)

/// Write the end of tag 's' to the string writer
let writeCloseTag (s : string) n a (stream : StringWriter) =
  stream.Write("</{0}>", s)

/// Recursively convert the Xml structure to text into the string writer
let rec writeXmlToStringWriter (Xml(nodes)) (stream : StringWriter) =
  for node in nodes do
    match node with
    | Element(s,n,a), children ->
      writeBeginTag s n a stream
      writeXmlToStringWriter children stream
      writeCloseTag s n a stream
    | Text(s), children ->
      stream.Write(s)
      writeXmlToStringWriter children stream
    | WhiteSpace(s), children ->
      stream.Write(s)
      writeXmlToStringWriter children stream

open Suave.Http

/// Read an attribute from the xml reader by moving it
/// one step along and then yielding that value.
let readatt (reader : XmlReader) = seq {
  while reader.MoveToNextAttribute() do
    yield (reader.Name,reader.Value)
}

/// Read through the xml reader, appending the elements of the XML as you
/// go to the value in the XML(l) input. Returns the read XML as an XML value.
let rec parser (reader : XmlReader) (Xml(l) as k) =
  if reader.Read() then
    match reader.NodeType with
    | XmlNodeType.Element ->
      let name  = reader.LocalName
      let attrs = readatt reader |> Seq.toArray
      if reader.IsEmptyElement then
        parser reader (Xml( l@ [Element(name,reader.Prefix,attrs),Xml([])]))
      else
        parser reader (Xml(l @ [Element(name,reader.Prefix,attrs),parser reader (Xml([]))]))
    | XmlNodeType.EndElement -> k
    | XmlNodeType.Text       -> parser reader (Xml(l @ [Text(reader.Value),Xml([])]))
    | XmlNodeType.Whitespace -> parser reader (Xml(l @ [WhiteSpace(reader.Value),Xml([])]))
    | XmlNodeType.Comment    -> parser reader k
    | x                      -> Xml(l)
  else
    k

/// Process the template/html that is the requested LocalPath in the request Uri,
/// binding the 'user' elements to values from the 'data' bindings. If the processing
/// is successful, return the data as UTF-8 string in the response body; otherwise
/// write the exception as a string to a 500 Internal Error response.
let processTemplate (data : Map<string,Binder>) ({ request = httpRequest; runtime = runtime } as ctx : HttpContext) =
  try
    let xmlReader = new XmlTextReader(Files.resolvePath runtime.homeDirectory httpRequest.url.AbsolutePath)
    xmlReader.Namespaces <- false

    let transform = parser xmlReader (Xml [])

    let xml = extractTemplate "user" data transform
    let sb = new StringBuilder()
    let str = new StringWriter(sb)
    writeXmlToStringWriter xml str
    let output = sb.ToString()
    Successful.OK output ctx
  with
  | x -> ServerErrors.INTERNAL_ERROR (x.ToString()) ctx
