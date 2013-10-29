namespace Suave
  module Log = begin
    val private ic : System.Globalization.CultureInfo
    val private sync_root : System.Object
    val log : format:Printf.StringFormat<'a,unit> -> 'a
  end
