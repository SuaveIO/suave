namespace Suave
  module Html = begin
    type Element =
      | Element of string * string * (string * string) []
      | Text of string
      | WhiteSpace of string
    type Xml = | Xml of Node list
    and Node = Element * Xml
    val flatMap : f:('a -> Xml) -> (seq<'a> -> Xml)
    val readatt : reader:System.Xml.XmlReader -> seq<string * string>
    val parser : reader:System.Xml.XmlReader -> Xml -> Xml
    type Binder = Xml -> Xml
    type BindParam =
      {name: string;
       calcValue: Xml -> Xml;}
    val submit : value:obj * func:(obj -> unit) -> s:Xml -> Xml
    val text_box : value:obj * func:(obj -> unit) -> s:Xml -> Xml
    val text : value:obj * func:(obj -> unit) -> s:Xml -> Xml
  end
