module Suave.Template

open System.Collections.Generic
open System.Xml
open System.IO

let extractTemplate (doc:XmlDocument) (datasource:IDictionary<string,Map<string,obj> seq>)  = 
    
    let targetDoc = new XmlDocument()
    
    let items = new Dictionary<string,Map<string,obj>>()
    
    let rec extractTemplate1 (nodes: XmlNodeList) (parent: XmlNode)  =
        for node in nodes do

            let attributes = node.Attributes
            let childNodes = node.ChildNodes
            
            match node.Name with
            |"far:repeat" -> for i in datasource.[attributes.["source"].Value] do
                                items.[attributes.["target"].Value] <- i
                                extractTemplate1 childNodes parent 
                                
            |"far:value" -> let index = attributes.["name"].Value
                            let src = attributes.["source"].Value
                            let textNode = targetDoc.CreateTextNode(items.[src].[index].ToString())
                            parent.AppendChild textNode |> ignore
                            
            | x -> let copy  = targetDoc.ImportNode(node,false)
                   parent.AppendChild copy |> ignore
                   extractTemplate1 childNodes copy 
               
    extractTemplate1 (doc.ChildNodes) targetDoc 
    targetDoc
    
let load_xml xml =
    let nt = new NameTable();
    let doc = new XmlDocument(nt)
    doc.PreserveWhitespace <- true
    let nsmgr = new XmlNamespaceManager(nt);
    nsmgr.AddNamespace("far", "http://codemaker.net/schema"); 
    doc.LoadXml(xml)
    doc      
    
let xml_to_string (xmlDoc:XmlDocument) = 
    let sw = new StringWriter();
    let xw = new XmlTextWriter(sw);
    xmlDoc.WriteTo(xw)
    sw.ToString()
    

open Web

let process_template data (http_request:HttpRequest) =
    let xml = File.ReadAllText(local_file http_request.Url) |> load_xml 
    let result = extractTemplate xml data
    ok (xml_to_string result) http_request    






