namespace Suave.Huffman

type HuffmanEncoding = unit -> int -> string

type Pin =
  | EndOfString
  | Forward of byte // -- node no.
  | GoBack  of byte * byte // -- node no and a decoded value
  | GoBack2 of byte * byte * byte //  node no and two decoded value

type WayStep = WayStep of (int option * Pin array)

type Way256 = WayStep array

type HTree =
  | Leaf of (int option * int)
  | Node of (int option * int * HTree * HTree)