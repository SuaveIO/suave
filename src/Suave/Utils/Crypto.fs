namespace Suave.Utils

/// Small crypto module that can do HMACs and generate random strings to use
/// as keys, as well as create a 'cryptobox'; i.e. a AES256+HMACSHA256 box with
/// compressed plaintext contents so that they can be easily stored in cookies.
module Crypto =
  open System
  open System.IO
  open System.IO.Compression
  open System.Text
  open System.Security.Cryptography

  /// The default hmac algorithm
  [<Literal>]
  let HMACAlgorithm = "HMACSHA256"

  /// The length of the HMAC value in number of bytes
  [<Literal>]
  let HMACLength = 32 // = 256 / 8

  /// Calculate the HMAC of the passed data given a private key
  let hmacAtOffset (key : byte []) offset count (data : byte[]) =
    use hmac = HMAC.Create(HMACAlgorithm)
    hmac.Key <- key
    hmac.ComputeHash (data, offset, count)

  let hmacOfBytes key (data : byte []) =
    hmacAtOffset key 0 (data.Length) data

  /// Calculate the HMAC value given the key
  /// and a seq of string-data which will be concatenated in its order and hmac-ed.
  let hmacOfText (key : byte []) (data : seq<string>) =
    hmacOfBytes key (String.Concat data |> UTF8.bytes)

  /// # bits in key
  let KeySize   = 256

  /// # bytes in key
  let KeyLength = KeySize / 8

  /// # bits in block
  let BlockSize = 128

  /// # bytes in IV
  /// 16 bytes for 128 bit blocks
  let IVLength = BlockSize / 8

  /// the global crypto-random pool for uniform and therefore cryptographically
  /// secure random values
  let cryptRandom = RandomNumberGenerator.Create()

  /// Fills the passed array with random bytes
  let randomize (bytes : byte []) =
    cryptRandom.GetBytes bytes
    bytes

  /// Generates a string key from the available characters with the given key size.
  let generateKey keyLength =
    Array.zeroCreate<byte> keyLength |> randomize

  let generateStdKey () =
    generateKey KeyLength

  let generateIV ivLength =
    Array.zeroCreate<byte> ivLength |> randomize

  let generateStdIV () =
    generateIV IVLength

  /// key: 32 bytes for 256 bit key
  /// Returns a new key and a new iv as two byte arrays as a tuple.
  let generateKeys () =
    generateStdKey (), generateStdIV ()

  type SecretboxEncryptionError =
    | InvalidKeyLength of string
    | EmptyMessageGiven

  type SecretboxDecryptionError =
    | TruncatedMessage of string
    | AlteredOrCorruptMessage of string

  let private secretboxInit key iv =
    let aes = new AesManaged()
    aes.KeySize   <- KeySize
    aes.BlockSize <- BlockSize
    aes.Mode      <- CipherMode.CBC
    aes.Padding   <- PaddingMode.PKCS7
    aes.IV        <- iv
    aes.Key       <- key
    aes

  let secretbox (key : byte []) (msg : byte []) =
    if key.Length <> KeyLength then
      Choice2Of2 (InvalidKeyLength (sprintf "key should be %d bytes but was %d bytes" KeyLength (key.Length)))
    elif msg.Length = 0 then
      Choice2Of2 EmptyMessageGiven
    else
      let iv  = generateStdIV ()
      use aes = secretboxInit key iv

      let mkCipherText (msg : byte []) (key : byte []) (iv : byte []) =
        use enc      = aes.CreateEncryptor(key, iv)
        use cipher   = new MemoryStream()
        use crypto   = new CryptoStream(cipher, enc, CryptoStreamMode.Write)
        let bytes = msg |> Encoding.gzipEncode
        crypto.Write (bytes, 0, bytes.Length)
        crypto.FlushFinalBlock()
        cipher.ToArray()

      use cipher_text = new MemoryStream()

      let bw  = new BinaryWriter(cipher_text)
      bw.Write iv
      bw.Write (mkCipherText msg key iv)
      bw.Flush ()

      let hmac = hmacOfBytes key (cipher_text.ToArray())
      bw.Write hmac
      bw.Dispose()

      Choice1Of2 (cipher_text.ToArray())

  let secretboxOfText (key : byte []) (msg : string) =
    secretbox key (msg |> UTF8.bytes)

  let secretboxOpen (key : byte []) (cipherText : byte []) =
    let hmacCalc = hmacAtOffset key 0 (cipherText.Length - HMACLength) cipherText
    let hmacGiven = Array.zeroCreate<byte> HMACLength
    Array.blit cipherText (cipherText.Length - HMACLength) // from
               hmacGiven  0                                 // to
               HMACLength                                    // # bytes for hmac

    if cipherText.Length < HMACLength + IVLength then
      Choice2Of2 (
        TruncatedMessage (
          sprintf "cipher text length was %d but expected >= %d"
                  cipherText.Length (HMACLength + IVLength)))
    elif not (Bytes.constantTimeCompare hmacCalc hmacGiven) then
      Choice2Of2 (AlteredOrCorruptMessage "calculated HMAC does not match expected/given")
    else
      let iv = Array.zeroCreate<byte> IVLength
      Array.blit cipherText 0
                 iv 0
                 IVLength
      use aes     = secretboxInit key iv
      use denc    = aes.CreateDecryptor(key, iv)
      use plain   = new MemoryStream()
      use crypto  = new CryptoStream(plain, denc, CryptoStreamMode.Write)
      crypto.Write(cipherText, IVLength, cipherText.Length - IVLength - HMACLength)
      crypto.FlushFinalBlock()
      Choice1Of2 (plain.ToArray() |> Encoding.gzipDecode)

  let secretboxOpenAsString keyText cipherText =
    secretboxOpen keyText cipherText |> Choice.map UTF8.toString
