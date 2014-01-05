module Suave.Html

open System
open System.Xml
open System.Text
open System.IO

/// Representation of the things that go into an HTML element
type Element =
  /// The element itself; a name, TODO SOMETHING and an array of attribute-value pairs.
  | Element of string * string * ( (string * string)[] )
  /// A text element inside the HTML element
  | Text of string
  /// Whitespace for formatting
  | WhiteSpace of string

/// XML is a list of nodes
type Xml = Xml of Node list
/// Each node has/is an element and then some other XML
and Node = Element * Xml

/// Construct a map from a to-xml transformer function and a sequence of values to be
/// placed in the XML. The resulting value are the transformed values, concatenated
/// into a list, placed in a XML node.
let flatMap f =
  Seq.map f
  >> Seq.map (fun (Xml y) -> y)
  >> Seq.concat
  >> Seq.toList
  >> Xml

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
    | x                      -> Log.logf "got: %A, name: %O" x x
                                Xml(l)
  else
    k

/// A binder takes XML, applies a transform, and returns the XML back.
type Binder = (Xml -> Xml)

/// A bind parameter is something named that can also be calculated/bound.
type BindParam = { name : string; calcValue : Xml -> Xml }

/// Create a submit element
let submit ((value:obj) ,(func: obj -> unit)) : Binder =
  (fun s ->  Xml([Element("input","",[| "type","submit"; "value",value.ToString(); "name","auto"|]), Xml([])]))

/// Create a text-box
let text_box ((value:obj) ,(func: obj -> unit)) : Binder =
  (fun s ->  Xml([Element("input","",[| "type","text"; "value",value.ToString(); "name","auto"|]), Xml([])]))

/// Create a text node in the HTML, i.e. plain text inside some XML.
let text ((value:obj) ,(func: obj -> unit)) : Binder =
  (fun s -> Xml([Text(value.ToString()),Xml([])]))
