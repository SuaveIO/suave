module Suave.Html

open System

type Attribute = string * string

type Element = string * Attribute[]
/// A Node in Html have the following forms
type Node =
  /// A regular html element that can contain a list of other nodes
  | Element of Element * Node list
  /// A void element is one that can't have content, like link, br, hr, meta
  /// See: https://www.w3.org/TR/html5/syntax.html#void-elements
  | VoidElement of Element
  /// A text value for a node
  | Text of string
  /// Whitespace for formatting
  | WhiteSpace of string

let tag tag attr (contents : Node list) = Element ((tag, Array.ofList attr), contents)
let voidTag tag attr = VoidElement (tag, Array.ofList attr)
let text s = [Text s]

let emptyText = text ""
let html = tag "html"
let head = tag "head"
let title attr s = tag "title" attr [Text s]
let script = tag "script"
let body = tag "body"
let div = tag "div"
let p = tag "p"
let a href attr = tag "a" (("href",href)::attr)
let span = tag "span"

// Void tags
let link attr = voidTag "link" attr
let meta attr = voidTag "meta" attr
let hr attr = voidTag "hr" attr
let br attr = voidTag "br" attr
let img attr = voidTag "img" attr
let input attr = voidTag "input" attr

/// Example

let samplePage =
  html [] [
    head [] [
      title [] "Little HTML DSL"
      link [ "rel", "https://instabt.com/instaBT.ico" ]
      script [ "type", "text/javascript"; "src", "js/jquery-2.1.0.min.js" ] []
      script [ "type", "text/javascript" ] (text "$().ready(function () { setup(); });" )
    ]
    body [] [
      div ["id","content"] [
        p [] (text "Hello world.")
        br []
        img [ "src", "http://fsharp.org/img/logo/fsharp256.png"]
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
