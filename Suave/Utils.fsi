namespace Suave
  module Utils = begin
    val succeed : x:'a -> 'a option
    val fail : 'a option
    val never : 'a -> 'b option
    val bind : (('a -> 'b option) -> 'a option -> 'b option)
    val delay : f:(unit -> 'a) -> 'a
    val ( >>= ) :
      a:('a -> 'b option) -> b:('b -> 'c option) -> x:'a -> 'c option
    val choose : options:('a -> 'b option) list -> arg:'a -> 'b option
    val warbler : f:('a -> 'a -> 'b) -> a:'a -> 'b
    val ( >>== ) :
      a:('a -> 'b option) -> b:('b -> 'b -> 'c option) -> ('a -> 'c option)
    val look_up :
      target:System.Collections.Generic.IDictionary<'b,'a> ->
        key:'b -> 'a option
    val ( ? ) :
      target:System.Collections.Generic.IDictionary<'b,'a> ->
        key:'b -> 'a option
    val ( ?<- ) :
      target:System.Collections.Generic.IDictionary<string,'a> ->
        key:string -> value:'a -> unit
    val opt : _arg1:'a option -> 'a
    val cnst : x:'a -> 'b -> 'a
    val cond : d:'a option -> f:('a -> 'b -> 'c) -> g:('b -> 'c) -> a:'b -> 'c
    val read_fully : input:System.IO.Stream -> byte []
    val encode_base64 : s:string -> string
    val decode_base64 : s:string -> string
    val bytes : s:string -> byte []
    val bytes_utf8 : s:string -> byte []
    val EOL : byte []
    val async_writeln : stream:System.IO.Stream -> s:string -> Async<unit>
    val async_writebytes : stream:System.IO.Stream -> b:byte [] -> Async<unit>
    val unblock : f:(unit -> 'a) -> Async<'a>
    type Async with
      static member AsyncRaise : e:#exn -> Async<'b>
    type Async with
      static member AwaitTask : t:System.Threading.Tasks.Task -> Async<unit>
    type AsyncBuilder with
      member
        Bind : t:System.Threading.Tasks.Task * f:(unit -> Async<'R>) ->
                 Async<'R>
    val transfer :
      to_stream:System.IO.Stream -> from:System.IO.Stream -> Async<unit>
    val init_next : p:'a [] -> int [] when 'a : equality
    val kmp : p:'a [] -> ('a [] -> int option) when 'a : equality
  end

