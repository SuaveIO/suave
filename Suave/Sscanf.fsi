namespace Suave
  module Sscanf = begin
    val private check : f:(string -> bool) -> x:string -> string
    val private parse_decimal : x:string -> decimal
    val parsers : System.Collections.Generic.IDictionary<char,(string -> obj)>
    val separators : string []
    val get_formatters : xs:char list -> char list
    val sscanf : pf:PrintfFormat<'a,'b,'c,'d,'t> -> s:string -> 't
    module private BasicTesting = begin
      val b : decimal
      val a : string
      val z : string
      val y : string
      val x : string
      val i : int
      val h : int
      val g : int
      val f : int
      val e : int
      val d : int
      val c : bool
      val p : char
      val o : float
      val n : float
      val m : float
      val l : float
      val k : float
      val j : float
      val aa : string
    end
  end
