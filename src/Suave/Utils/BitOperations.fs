namespace Suave.Utils

  module BitOperations =

    let isset (x:byte) i = x &&& (1uy <<< i) <> 0uy
    let set   (x:byte) i = x ||| (1uy <<< i)
    let unset (x:byte) i = x &&& ~~~(1uy <<< i)

