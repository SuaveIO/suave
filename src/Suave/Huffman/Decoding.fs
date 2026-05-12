namespace Suave.Huffman

module Decoding =

  open System
  open System.IO
  open Bits

  let idxEos = 256

  let rec mark i bs t =
    match i, bs, t with
    | i, []     , Leaf (None, v)       -> Leaf (Some i, v)
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

  let eosInfo = function
    | Leaf(mx, _ ) -> mx
    | Node(mx, _, _, _ ) -> mx

  type Chara = Nothing | One of byte | Two of (byte*byte)

  let inc (a:Chara) w =
    match a with
    | Nothing -> One w
    | One v -> Two (v, w)
    | _ -> failwith "inc error."

  let rec step (root:HTree) (t:HTree) (char:Chara) (bss: Bits) =
    match t, char, bss with
    | Leaf (_,v), x, bss ->
      if v = idxEos then
        EndOfString
      else
        let w = byte v
        let x' = inc x w
        step root root x' bss
    | Node (_,n,_,_), Nothing  , [] -> Forward(byte n)
    | Node (_,n,_,_), One w    , [] -> GoBack(byte n,w)
    | Node (_,n,_,_), Two (w,z), [] -> GoBack2(byte n,w,z)
    | Node (_,_,l,_), mx, F::tail   -> step root l mx tail
    | Node (_,_,_,r), mx, T::tail   -> step root r mx tail

  let rec flatten (decoder : HTree) : HTree list =
    match decoder with
    | Leaf (_, _) ->
      []
    | Node (_, _, l, r) as t ->
      t:: (flatten l @ flatten r)

  let construct (decoder : HTree) : Way256 =
    let to16ways x = WayStep ((eosInfo x), (List.map (step decoder x Nothing) bits8s) |> List.toArray)
    List.map to16ways (flatten decoder) |> List.toArray

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
        | WayStep (None,_) -> failwith "illegal eos"
      else
        let w = rbuf.ReadByte()
        let way = doit way0 w
        go (n-1) way

    go len (way256.[0])

  let decode (buf : byte array) size rbuf len =
    let wbuf = new MemoryStream(buf)
    dec wbuf rbuf len
    Array.sub buf 0 (int wbuf.Position)

