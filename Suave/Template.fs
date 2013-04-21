module Suave.Template

open System
open System.Collections.Generic
open System.Xml
open System.IO
open System.Reflection
open System.Text

open Suave.Web  
open Suave.Html
open Suave.Http

let load_object (str:string) =
    let entryAssembly = Assembly.GetEntryAssembly()
    entryAssembly.CreateInstance(str.Replace('.','+'))
    
let invoke object action args =
    let typ = object.GetType()
    let meth = typ.GetMethod(action,[| typeof<Xml>|])
    meth.Invoke(object,[| args |])
    
let (|Pat|NoPat|) (x:String) = 
    if x.Contains(":") then 
        let index = x.IndexOf(':')
        Pat(x.Substring(0,index),x.Substring(index+1))
    else NoPat

let extractTemplate (nsx:string) (scope:Map<string,Binder>) (xml:Xml)   = 

    let rec extractTemplate' (Xml(nodelist)) (Xml(result)) : Xml  =
        
        let  substitute (node:Node) =
            
            match node with
            |(Element(Pat(ns,name),_,att),childs) 
                when ns = nsx ->
                       
                            if scope.ContainsKey(name) then
                                
                                let func = scope.[name] 
                                let Xml(nodes) as f = func childs
                                f
                            else failwith (sprintf "invalid binding name: '%s'\n" name)
                            
                       
            |(Element(Pat("lift",name),_,att),childs)  -> 
                        let obj = load_object name
                        let action = Array.find (fun (x,_) -> x="action") att |> snd
                        (invoke obj action (childs))  :?> Xml
                        
                        
            |(elem,childs)-> Xml([elem,extractTemplate' childs (Xml[])])
        
        let rec loop (nl:Node list) (Xml(acc) as res) = 
            match nl with 
            |[] -> res
            |p::tail -> 
                let Xml(subst) as s = substitute p 
                loop tail (Xml(acc @ (subst)))
            
        loop nodelist (Xml([]))    
    extractTemplate' (xml) (Xml([]))

let bind (ns: string, node: Xml,  binds: (Map<string,Binder>) ) = 
    extractTemplate ns (binds) node 
   
type Attributes = (string*string)[]

let write_begin_tag (s:string) n (a:(string*string)[]) (stream:StringWriter) = 
    let tt = Array.fold( fun z (x,y)  -> z + (sprintf "%s=\"%s\" " x y) ) " " a
    if tt=" " then stream.Write("<{0}>",s);
    else stream.Write("<{0} {1}>",s,tt);
    
let write_close_tag (s:string) n a (stream:StringWriter) = 
    stream.Write("</{0}>",s);    
    
let rec xml_to_string1 (Xml(nodes)) (stream:StringWriter) =         
    for node in nodes do
        match node with
        |Element(s,n,a),childs 
            -> 
                write_begin_tag s n a stream
                xml_to_string1 childs stream
                write_close_tag s n a stream
        |Text(s),childs -> stream.Write(s);xml_to_string1 childs stream
        |WhiteSpace(s),childs -> stream.Write(s);xml_to_string1 childs stream
    
let process_template (data:Map<string,Binder>) (http_request:HttpRequest) =
    try 
        let xmlReader = new XmlTextReader(local_file http_request.Url)
        xmlReader.Namespaces <- false
        
        let transform = parser xmlReader (Xml([]))
        
        let xml = extractTemplate "user" data ( transform) 
        let sb = new StringBuilder()
        let str = new StringWriter(sb)
        xml_to_string1 xml str
        let output = sb.ToString()
        ok (fun _ -> bytes output) http_request    
    with
    |x -> failure (fun _ -> bytes (x.ToString())) http_request    






