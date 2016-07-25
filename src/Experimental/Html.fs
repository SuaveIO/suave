module Suave.Html

open System

type Attribute = string * string

type Element = string * Attribute[]
/// A Node in Html have the following forms
type Node =
  /// A regular html element that can contain a list of other nodes
  | Element of Element * Node list
  /// A void element is one that can't have content, like link, br, hr, meta
  /// See: https://dev.w3.org/html5/html-author/#void
  | VoidElement of Element
  /// A text value for a node
  | Text of string
  /// Whitespace for formatting
  | WhiteSpace of string

let tag tag attr (contents : Node list) = Element ((tag, Array.ofList attr), contents)
let voidTag tag attr = VoidElement (tag, Array.ofList attr)
let text s = [Text s]
let empty:Node list = []

let emptyText = text ""

let htmlAttr = tag "html"
let html nodes = htmlAttr [ ] nodes

let headAttr = tag "head"
let head nodes = headAttr [ ] nodes

let titleAttr attr s = tag "title" attr [(Text s)]
let title = titleAttr [ ]

let scriptAttr = tag "script"
let script = scriptAttr [ ]

let bodyAttr = tag "body"
let body = bodyAttr [ ]

let divAttr = tag "div"
let div = divAttr [ ]

let pAttr = tag "p"
let p = pAttr [ ]

let aAttr href attr = tag "a" (("href",href)::attr)
let a href = aAttr href [ ]

let spanAttr = tag "span"
let span  = spanAttr [ ]

let imgAttr attr = tag "img" attr empty
let img  = imgAttr [ ]

let inputAttr attr = tag "input" attr empty
let input = inputAttr [ ]

// Void tags
let linkAttr attr = voidTag "link" attr
let link = linkAttr [ ]

let metaAttr attr = voidTag "meta" attr
let meta = metaAttr [ ]

let hrAttr attr = voidTag "hr" attr
let hr = hrAttr [ ]

let brAttr attr = voidTag "br" attr
let br = brAttr [ ]

/// Example

let samplePage =
  html [
    head [
      title "Little HTML DSL"
      linkAttr [ "rel", "https://instabt.com/instaBT.ico" ]
      scriptAttr [ "type", "text/javascript"; "src", "js/jquery-2.1.0.min.js" ] []
      scriptAttr [ "type", "text/javascript" ] (text "$().ready(function () { setup(); });" )
    ]
    body [
      divAttr ["id","content"] [
        p (text "Hello world.")
        br
        imgAttr [ "src", "http://fsharp.org/img/logo/fsharp256.png"]
      ]
    ]
 ]

/// Rendering

let rec htmlToString node =

  let startElemToString (e, attributes) =
    match attributes with
    | [||] -> sprintf "<%s>" e
    | xs ->
      let attributeString =
        attributes
        |> Array.map (fun (k,v) -> sprintf " %s=\"%s\"" k v)
        |> String.Concat
      sprintf "<%s%s>" e attributeString

  let endElemToString (e, _) = sprintf "</%s>" e

  match node with
  | Text text -> text
  | WhiteSpace text -> text
  | Element (e, nodes) ->
    let inner = nodes |> List.map htmlToString |> String.Concat
    let startTag = e |> startElemToString
    let endTag = e |> endElemToString
    sprintf "%s%s%s" startTag inner endTag
  | VoidElement e -> e |> startElemToString

let renderHtmlDocument document =
  sprintf "<!DOCTYPE html>%s%s" (Environment.NewLine) (document |> htmlToString)
///
///let sample = samplePage |> htmlToString
///
