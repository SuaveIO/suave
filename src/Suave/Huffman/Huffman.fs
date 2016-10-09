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

module HuffmanDecoding =

  open System.IO

  open Bits

  let idxEos = 256

  let rec mark i bs t =
    match i, bs, t with
    | i, [], Leaf (None, v) -> Leaf (Some i, v)
    | i, F::tail, Node (None, n, l, r) -> Node (Some i, n, mark (i + 1) tail l,r )
    | i, T::tail, Node (None, n, l, r) -> Node (Some i, n, l, mark (i + 1) tail r )
    | _, _, _ -> failwith "error: mark"

  let partition p xs = (List.filter p xs, List.filter (fun x -> not (p x)) xs)


  let rec build cnt0 pairs =
    match pairs with
    | [(v,[])] -> (cnt0,Leaf (None, v))
    | xs ->
      let (fs',ts') = partition (fun x -> List.head(snd x) = F) xs
      let fs = List.map (fun (a,b) ->  a, List.tail b) fs'
      let ts = List.map (fun (a,b) ->  a, List.tail b) ts'
      let cnt1,l = build (cnt0 + 1) fs
      let cnt2,r = build cnt1 ts
      (cnt2, Node (None, cnt0, l, r))

  let toHTree (bs: Bits list) : HTree =
    let eos = bs.[idxEos]
    mark 1 eos (snd (build 0 (List.zip [0 .. idxEos] bs)))

  let next (WayStep (_, a16)) w = a16.[w]

  let bits8arr = List.toArray bits8s

  let eosInfo = function
    | Leaf(mx, _ ) -> mx
    | Node(mx, _, _, _ ) -> mx

  type Chara = None | One of byte | Two of (byte*byte)

  let rec step root t char bss =
    match t, char, bss with
    |  Leaf (_,v), x, bss ->
      if v = idxEos then
        EndOfString
      else
        let w = v
        EndOfString

  let construct (decoder : HTree) : Way256 =
    let to16ways x = WayStep ((eosInfo x), (Array.map (step decoder x None) bits8arr))

    Array.create 256 (to16ways decoder)

  let way256 : Way256 = construct (toHTree Table.huffmanTable)

  let dec (wbuf: MemoryStream) (rbuf: MemoryStream) len =
    let doit way w =
      match next way w with
      | EndOfString -> failwith "eos is in the middle"
      | Forward n -> way256.[int n]
      | GoBack (n,v) ->
        wbuf.WriteByte v
        way256.[int n]
      | GoBack2 (n,v1,v2) ->
        wbuf.WriteByte v1
        wbuf.WriteByte v2
        way256.[int n]

    let rec go n way0 =
      if n = 0 then
        match way0 with
        | WayStep (Some i, _) ->
          if i <= 8 then ()
          else failwith "too long eos"
        | WayStep (_,_) -> failwith "illegal eos"
      else
        let w = rbuf.ReadByte()
        let way = doit way0 w
        go (n-1) way

    go len (way256.[0])