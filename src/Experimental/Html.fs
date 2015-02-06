module Suave.Html

open System

type Attribute = string * string

/// Representation of the things that go into an HTML element
type Element =
  /// The element itself; a name, a xml namespace and an array of attribute-value pairs.
  | Element of string * string * (Attribute array)
  /// A text element inside the HTML element
  | Text of string
  /// Whitespace for formatting
  | WhiteSpace of string

/// XML is a list of nodes
type Xml = Xml of Node list
/// Each node has/is an element and then some other XML
and Node = Element * Xml

let tag tag attr (contents : Xml) = Xml [ (Element (tag,"", attr), contents) ]

let empty = Xml[]

let text s = Xml([Text s, Xml[]])

let empty_text = text ""

/// HTML elements.
/// If you need to pass attributes use the version sufixed by ` (funny quote symbol)

let html' = tag "html"
let html  = html' Array.empty<Attribute>

let head' = tag "head"
let head  = head' Array.empty<Attribute>

let title' attr s = tag "title" attr (Xml([Text s,Xml []]))
let title  = title' Array.empty<Attribute>

let link' attr = tag "link" attr empty
let link  = link' Array.empty<Attribute>

let script' = tag "script"
let script  = script' Array.empty<Attribute>

let body' = tag "body"
let body  = body' Array.empty<Attribute>

let div' = tag "div"
let div  = div' Array.empty<Attribute>

let p' = tag "p"
let p  = p' Array.empty<Attribute>

let span' = tag "span"
let span  = span' Array.empty<Attribute>

let img' attr = tag "img" attr empty
let img  = img' Array.empty<Attribute>

let br' attr = tag "br" attr empty
let br = br' Array.empty<Attribute>

let input' attr = tag "input" attr empty
let input = input' Array.empty<Attribute>

/// Flattens an XML list
let flatten = List.map (fun (Xml y) -> y) >> List.concat >> Xml

/// `flatten` operator
let (<%) f xxs = f (flatten xxs)

/// Example

let sample_page = 
  html <% [ 
    head <% [
      title "Little HTML DSL"
      link' [| "rel", "https://instabt.com/instaBT.ico" |]
      script' [| "type", "text/javascript"; "src", "js/jquery-2.1.0.min.js" |] empty_text
      script' [| "type", "text/javascript" |]
       <| text "$().ready(function () { setup(); });"
    ] 
    body (
      div' [|"id","content"|] <% [ 
        p <| text "Hello world."
        br
        img' [| "src", "http://fsharp.org/img/logo/fsharp256.png"|]
      ])
    ]

/// Rendering

let leaf_element_to_string = function
 | Text text -> text
 | WhiteSpace text -> text
 | Element (e,id, attributes) ->
   if Array.length attributes = 0 then
     sprintf "<%s/>" e
   else
     let ats = 
       Array.map (fun (a,b) -> sprintf "%s=\"%s\"" a b) attributes
       |> String.Concat
     sprintf "<%s %s/>" e ats

let begin_element_to_string = function
 | Text text -> failwith "invalid option"
 | WhiteSpace text -> failwith "invalid option"
 | Element (e,id, attributes) ->
   if Array.length attributes = 0 then
     sprintf "<%s>" e
   else
     let ats = 
       Array.map (fun (a,b) -> sprintf "%s=\"%s\"" a b) attributes
       |> String.Concat
     sprintf "<%s %s>" e ats

let end_element_to_string = function
 | Text text -> failwith "invalid option"
 | WhiteSpace text -> failwith "invalid option"
 | Element (e,_, _) ->
   sprintf "</%s>" e

let rec node_to_string (element : Element, xml) =
  match xml with
  | Xml [] -> leaf_element_to_string element
  | _  ->
    let inner = xml_to_string xml
    (begin_element_to_string element) + inner + (end_element_to_string element)
and xml_to_string (Xml xml) =
  String.Concat (List.map node_to_string xml)

///
/// let html = sample_page |> xml_to_string
///