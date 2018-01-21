module Suave.Xml

open System

type Attribute = string * string

/// Representation of the things that go into an HTML element
type Element =
  /// The element itself; a name, a xml namespace and an array of attribute-value pairs.
  | Element of string * string * Attribute[]
  /// A text element inside the HTML element
  | Text of string
  /// Whitespace for formatting
  | WhiteSpace of string

/// XML is a list of nodes
type Xml = Xml of Node list
/// Each node has/is an element and then some other XML
and Node = Element * Xml

let tag tag attr (contents : Xml) = Xml [ (Element (tag,"", Array.ofList attr), contents) ]

let empty = Xml[]

let text s = Xml([Text s, Xml[]])

let emptyText = text ""

/// HTML elements.
/// If you need to pass attributes use the version sufixed by Attr

/// Flattens an XML list
let flatten xs = xs |> List.map (fun (Xml y) -> y) |> List.concat |> Xml


let htmlAttr attr s = tag "html" attr s
let html xs = htmlAttr [ ] (flatten xs)

let headAttr attr s = tag "head" attr s
let head xs = headAttr [ ] (flatten xs)

let titleAttr attr s = tag "title" attr (Xml([Text s,Xml []]))
let title  = titleAttr [ ]

let linkAttr attr = tag "link" attr empty
let link  = linkAttr [ ]

let scriptAttr attr x = tag "script" attr (flatten x)
let script  = scriptAttr [ ]

let bodyAttr attr x = tag "body" attr (flatten x)
let body  = bodyAttr [ ]

let divAttr attr x = tag "div" attr (flatten x)
let div  = divAttr [ ]

let pAttr attr x = tag "p" attr (flatten x)
let p  = pAttr [ ]

let spanAttr attr x = tag "span" attr x
let span  = spanAttr [ ]

let imgAttr attr = tag "img" attr empty
let img  = imgAttr [ ]

let brAttr attr = tag "br" attr empty
let br = brAttr [ ]

let inputAttr attr = tag "input" attr empty
let input = inputAttr [ ]

/// Example

let samplePage =
  html [
    head [
      title "Little HTML DSL"
      linkAttr [ "rel", "https://instabt.com/instaBT.ico" ]
      scriptAttr [ "type", "text/javascript"; "src", "js/jquery-2.1.0.min.js" ] []
      scriptAttr [ "type", "text/javascript" ] [ text "$().ready(function () { setup(); });" ]
    ]
    body [
      divAttr ["id","content"] [
        p [ text "Hello world." ]
        br
        imgAttr [ "src", "http://fsharp.org/img/logo/fsharp256.png"]
      ]
    ]
 ]

/// Rendering

let internal joinSpaces (strings:#seq<string>) =
  String.Join (" ", strings)

let internal attributesToString attributes =
  attributes
  |> Array.map (fun (a, b) -> sprintf "%s=\"%s\"" a b) 
  |> joinSpaces

let internal leafElementToString = function
 | Text text -> text
 | WhiteSpace text -> text
 | Element (e, id, attributes) ->
   match attributes with
   | [||] -> sprintf "<%s/>" e
   | _ -> sprintf "<%s %s/>" e (attributes |> attributesToString)

let internal beginElementToString = function
 | Text text -> failwith "invalid option"
 | WhiteSpace text -> failwith "invalid option"
 | Element (e, id, attributes) ->
   match attributes with
   | [||] -> sprintf "<%s>" e
   | _ -> sprintf "<%s %s>" e (attributes |> attributesToString)

let internal endElementToString = function
 | Text text -> failwith "invalid option"
 | WhiteSpace text -> failwith "invalid option"
 | Element (e,_, _) ->
   sprintf "</%s>" e

let rec internal nodeToString (element : Element, xml) =
  match xml with
  | Xml [] -> leafElementToString element
  | _  ->
    let inner = xmlToString xml
    (beginElementToString element) + inner + (endElementToString element)

and xmlToString (Xml xml) =
  String.Concat (List.map nodeToString xml)

///
/// let html = samplePage |> xmlToString
///
