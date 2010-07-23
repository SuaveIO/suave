module Suave.Html

open System
open System.Xml
open System.Text
open System.IO

type Element = 
    |Element of string*string*((string*string)[])
    |Text of string
    |WhiteSpace of string

type Xml = Xml of Node list 
and Node = Element * (Xml)

let flatMap f  =
    Seq.map f
    >> Seq.map (fun (Xml(y)) -> y)
    >> Seq.concat
    >> Seq.toList
    >> Xml

let readatt (reader:XmlReader) = seq {
    while reader.MoveToNextAttribute() do
        yield (reader.Name,reader.Value)
}

let rec parser (reader:XmlReader) (Xml(l) as k)=
    
    if reader.Read() then
        match reader.NodeType with
        |XmlNodeType.Element -> 
                    let name  = reader.LocalName
                    let attrs = readatt reader |> Seq.toArray
                    if reader.IsEmptyElement then
                        parser reader  (Xml( l@ [Element(name,reader.Prefix,attrs),Xml([])]))
                    else
                        parser reader (Xml(l @ [Element(name,reader.Prefix,attrs),parser reader (Xml([]))]))
        |XmlNodeType.EndElement -> k
        |XmlNodeType.Text -> parser reader (Xml(l @ [Text(reader.Value),Xml([])]))
        |XmlNodeType.Whitespace -> parser reader (Xml(l @ [WhiteSpace(reader.Value),Xml([])]))
        |XmlNodeType.Comment -> parser reader k
        |x -> printf "got:%A, name:%s\n" x (x.ToString());Xml(l)
    else
        k
        
type Binder = (Xml -> Xml)

type BindParam = { name:string; calcValue:(Xml -> Xml) }

let submit ((value:obj) ,(func: obj -> unit)) : Binder =
    (fun s ->  Xml([Element("input","",[| "type","submit"; "value",value.ToString(); "name","auto"|]),Xml([])]))

let text_box ((value:obj) ,(func: obj -> unit)) : Binder =
    (fun s ->  Xml([Element("input","",[| "type","text"; "value",value.ToString(); "name","auto"|]),Xml([])]))

let text ((value:obj) ,(func: obj -> unit)) : Binder =
    (fun s -> Xml([Text(value.ToString()),Xml([])]))