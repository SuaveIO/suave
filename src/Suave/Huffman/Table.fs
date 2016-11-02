namespace Suave.Huffman

module Table =

  open Suave.Huffman.Bits

  let huffmanTable : Bits list  = [
    [T;T;T;T;T;T;T;T;T;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T]
    [F;T;F;T;F;F]
    [T;T;T;T;T;T;T;F;F;F]
    [T;T;T;T;T;T;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;F;F;T]
    [F;T;F;T;F;T]
    [T;T;T;T;T;F;F;F]
    [T;T;T;T;T;T;T;T;F;T;F]
    [T;T;T;T;T;T;T;F;T;F]
    [T;T;T;T;T;T;T;F;T;T]
    [T;T;T;T;T;F;F;T]
    [T;T;T;T;T;T;T;T;F;T;T]
    [T;T;T;T;T;F;T;F]
    [F;T;F;T;T;F]
    [F;T;F;T;T;T]
    [F;T;T;F;F;F]
    [F;F;F;F;F]
    [F;F;F;F;T]
    [F;F;F;T;F]
    [F;T;T;F;F;T]
    [F;T;T;F;T;F]
    [F;T;T;F;T;T]
    [F;T;T;T;F;F]
    [F;T;T;T;F;T]
    [F;T;T;T;T;F]
    [F;T;T;T;T;T]
    [T;F;T;T;T;F;F]
    [T;T;T;T;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;F;F]
    [T;F;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;F;T;T]
    [T;T;T;T;T;T;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;F;T;F]
    [T;F;F;F;F;T]
    [T;F;T;T;T;F;T]
    [T;F;T;T;T;T;F]
    [T;F;T;T;T;T;T]
    [T;T;F;F;F;F;F]
    [T;T;F;F;F;F;T]
    [T;T;F;F;F;T;F]
    [T;T;F;F;F;T;T]
    [T;T;F;F;T;F;F]
    [T;T;F;F;T;F;T]
    [T;T;F;F;T;T;F]
    [T;T;F;F;T;T;T]
    [T;T;F;T;F;F;F]
    [T;T;F;T;F;F;T]
    [T;T;F;T;F;T;F]
    [T;T;F;T;F;T;T]
    [T;T;F;T;T;F;F]
    [T;T;F;T;T;F;T]
    [T;T;F;T;T;T;F]
    [T;T;F;T;T;T;T]
    [T;T;T;F;F;F;F]
    [T;T;T;F;F;F;T]
    [T;T;T;F;F;T;F]
    [T;T;T;T;T;T;F;F]
    [T;T;T;F;F;T;T]
    [T;T;T;T;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;F;F]
    [T;F;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;F;T]
    [F;F;F;T;T]
    [T;F;F;F;T;T]
    [F;F;T;F;F]
    [T;F;F;T;F;F]
    [F;F;T;F;T]
    [T;F;F;T;F;T]
    [T;F;F;T;T;F]
    [T;F;F;T;T;T]
    [F;F;T;T;F]
    [T;T;T;F;T;F;F]
    [T;T;T;F;T;F;T]
    [T;F;T;F;F;F]
    [T;F;T;F;F;T]
    [T;F;T;F;T;F]
    [F;F;T;T;T]
    [T;F;T;F;T;T]
    [T;T;T;F;T;T;F]
    [T;F;T;T;F;F]
    [F;T;F;F;F]
    [F;T;F;F;T]
    [T;F;T;T;F;T]
    [T;T;T;F;T;T;T]
    [T;T;T;T;F;F;F]
    [T;T;T;T;F;F;T]
    [T;T;T;T;F;T;F]
    [T;T;T;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;F;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;F;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;T]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;F;F;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;F;T;T;T;F]
    [T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T;T]
  ]

  let huffmanTable' : uint64 list= 
    [
    0x1ff8UL
  ; 0x7fffd8UL
  ; 0xfffffe2UL
  ; 0xfffffe3UL
  ; 0xfffffe4UL
  ; 0xfffffe5UL
  ; 0xfffffe6UL
  ; 0xfffffe7UL
  ; 0xfffffe8UL
  ; 0xffffeaUL
  ; 0x3ffffffcUL
  ; 0xfffffe9UL
  ; 0xfffffeaUL
  ; 0x3ffffffdUL
  ; 0xfffffebUL
  ; 0xfffffecUL
  ; 0xfffffedUL
  ; 0xfffffeeUL
  ; 0xfffffefUL
  ; 0xffffff0UL
  ; 0xffffff1UL
  ; 0xffffff2UL
  ; 0x3ffffffeUL
  ; 0xffffff3UL
  ; 0xffffff4UL
  ; 0xffffff5UL
  ; 0xffffff6UL
  ; 0xffffff7UL
  ; 0xffffff8UL
  ; 0xffffff9UL
  ; 0xffffffaUL
  ; 0xffffffbUL
  ; 0x14UL
  ; 0x3f8UL
  ; 0x3f9UL
  ; 0xffaUL
  ; 0x1ff9UL
  ; 0x15UL
  ; 0xf8UL
  ; 0x7faUL
  ; 0x3faUL
  ; 0x3fbUL
  ; 0xf9UL
  ; 0x7fbUL
  ; 0xfaUL
  ; 0x16UL
  ; 0x17UL
  ; 0x18UL
  ; 0x0UL
  ; 0x1UL
  ; 0x2UL
  ; 0x19UL
  ; 0x1aUL
  ; 0x1bUL
  ; 0x1cUL
  ; 0x1dUL
  ; 0x1eUL
  ; 0x1fUL
  ; 0x5cUL
  ; 0xfbUL
  ; 0x7ffcUL
  ; 0x20UL
  ; 0xffbUL
  ; 0x3fcUL
  ; 0x1ffaUL
  ; 0x21UL
  ; 0x5dUL
  ; 0x5eUL
  ; 0x5fUL
  ; 0x60UL
  ; 0x61UL
  ; 0x62UL
  ; 0x63UL
  ; 0x64UL
  ; 0x65UL
  ; 0x66UL
  ; 0x67UL
  ; 0x68UL
  ; 0x69UL
  ; 0x6aUL
  ; 0x6bUL
  ; 0x6cUL
  ; 0x6dUL
  ; 0x6eUL
  ; 0x6fUL
  ; 0x70UL
  ; 0x71UL
  ; 0x72UL
  ; 0xfcUL
  ; 0x73UL
  ; 0xfdUL
  ; 0x1ffbUL
  ; 0x7fff0UL
  ; 0x1ffcUL
  ; 0x3ffcUL
  ; 0x22UL
  ; 0x7ffdUL
  ; 0x3UL
  ; 0x23UL
  ; 0x4UL
  ; 0x24UL
  ; 0x5UL
  ; 0x25UL
  ; 0x26UL
  ; 0x27UL
  ; 0x6UL
  ; 0x74UL
  ; 0x75UL
  ; 0x28UL
  ; 0x29UL
  ; 0x2aUL
  ; 0x7UL
  ; 0x2bUL
  ; 0x76UL
  ; 0x2cUL
  ; 0x8UL
  ; 0x9UL
  ; 0x2dUL
  ; 0x77UL
  ; 0x78UL
  ; 0x79UL
  ; 0x7aUL
  ; 0x7bUL
  ; 0x7ffeUL
  ; 0x7fcUL
  ; 0x3ffdUL
  ; 0x1ffdUL
  ; 0xffffffcUL
  ; 0xfffe6UL
  ; 0x3fffd2UL
  ; 0xfffe7UL
  ; 0xfffe8UL
  ; 0x3fffd3UL
  ; 0x3fffd4UL
  ; 0x3fffd5UL
  ; 0x7fffd9UL
  ; 0x3fffd6UL
  ; 0x7fffdaUL
  ; 0x7fffdbUL
  ; 0x7fffdcUL
  ; 0x7fffddUL
  ; 0x7fffdeUL
  ; 0xffffebUL
  ; 0x7fffdfUL
  ; 0xffffecUL
  ; 0xffffedUL
  ; 0x3fffd7UL
  ; 0x7fffe0UL
  ; 0xffffeeUL
  ; 0x7fffe1UL
  ; 0x7fffe2UL
  ; 0x7fffe3UL
  ; 0x7fffe4UL
  ; 0x1fffdcUL
  ; 0x3fffd8UL
  ; 0x7fffe5UL
  ; 0x3fffd9UL
  ; 0x7fffe6UL
  ; 0x7fffe7UL
  ; 0xffffefUL
  ; 0x3fffdaUL
  ; 0x1fffddUL
  ; 0xfffe9UL
  ; 0x3fffdbUL
  ; 0x3fffdcUL
  ; 0x7fffe8UL
  ; 0x7fffe9UL
  ; 0x1fffdeUL
  ; 0x7fffeaUL
  ; 0x3fffddUL
  ; 0x3fffdeUL
  ; 0xfffff0UL
  ; 0x1fffdfUL
  ; 0x3fffdfUL
  ; 0x7fffebUL
  ; 0x7fffecUL
  ; 0x1fffe0UL
  ; 0x1fffe1UL
  ; 0x3fffe0UL
  ; 0x1fffe2UL
  ; 0x7fffedUL
  ; 0x3fffe1UL
  ; 0x7fffeeUL
  ; 0x7fffefUL
  ; 0xfffeaUL
  ; 0x3fffe2UL
  ; 0x3fffe3UL
  ; 0x3fffe4UL
  ; 0x7ffff0UL
  ; 0x3fffe5UL
  ; 0x3fffe6UL
  ; 0x7ffff1UL
  ; 0x3ffffe0UL
  ; 0x3ffffe1UL
  ; 0xfffebUL
  ; 0x7fff1UL
  ; 0x3fffe7UL
  ; 0x7ffff2UL
  ; 0x3fffe8UL
  ; 0x1ffffecUL
  ; 0x3ffffe2UL
  ; 0x3ffffe3UL
  ; 0x3ffffe4UL
  ; 0x7ffffdeUL
  ; 0x7ffffdfUL
  ; 0x3ffffe5UL
  ; 0xfffff1UL
  ; 0x1ffffedUL
  ; 0x7fff2UL
  ; 0x1fffe3UL
  ; 0x3ffffe6UL
  ; 0x7ffffe0UL
  ; 0x7ffffe1UL
  ; 0x3ffffe7UL
  ; 0x7ffffe2UL
  ; 0xfffff2UL
  ; 0x1fffe4UL
  ; 0x1fffe5UL
  ; 0x3ffffe8UL
  ; 0x3ffffe9UL
  ; 0xffffffdUL
  ; 0x7ffffe3UL
  ; 0x7ffffe4UL
  ; 0x7ffffe5UL
  ; 0xfffecUL
  ; 0xfffff3UL
  ; 0xfffedUL
  ; 0x1fffe6UL
  ; 0x3fffe9UL
  ; 0x1fffe7UL
  ; 0x1fffe8UL
  ; 0x7ffff3UL
  ; 0x3fffeaUL
  ; 0x3fffebUL
  ; 0x1ffffeeUL
  ; 0x1ffffefUL
  ; 0xfffff4UL
  ; 0xfffff5UL
  ; 0x3ffffeaUL
  ; 0x7ffff4UL
  ; 0x3ffffebUL
  ; 0x7ffffe6UL
  ; 0x3ffffecUL
  ; 0x3ffffedUL
  ; 0x7ffffe7UL
  ; 0x7ffffe8UL
  ; 0x7ffffe9UL
  ; 0x7ffffeaUL
  ; 0x7ffffebUL
  ; 0xffffffeUL
  ; 0x7ffffecUL
  ; 0x7ffffedUL
  ; 0x7ffffeeUL
  ; 0x7ffffefUL
  ; 0x7fffff0UL
  ; 0x3ffffeeUL
  ; 0x3fffffffUL
  ]
