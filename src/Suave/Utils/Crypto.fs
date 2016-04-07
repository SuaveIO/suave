/// Small crypto module that can do HMACs and generate random strings to use
/// as keys, as well as create a 'cryptobox'; i.e. a AES256+HMACSHA256 box with
/// compressed plaintext contents so that they can be easily stored in cookies.
module Suave.Utils.Crypto

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
let HMACLength = 32us // = 256 / 8

/// Calculate the HMAC of the passed data given a private key
let hmacAtOffset (key : byte []) offset count (data : byte[]) =
#if NETSTANDARD1_5
  use hmac = new HMACSHA256()
#else
  use hmac = HMAC.Create(HMACAlgorithm)
#endif
  hmac.Key <- key
  hmac.ComputeHash (data, offset, count)

let hmacOfBytes key (data : byte []) =
  hmacAtOffset key 0 (data.Length) data

/// Calculate the HMAC value given the key
/// and a seq of string-data which will be concatenated in its order and hmac-ed.
let hmacOfText (key : byte []) (data : seq<string>) =
  hmacOfBytes key (String.Concat data |> UTF8.bytes)

/// # bits in key
let KeySize   = 256us

/// # bytes in key
let KeyLength = KeySize / 8us

/// # bits in block
let BlockSize = 128us

/// # bytes in IV
/// 16 bytes for 128 bit blocks
let IVLength = BlockSize / 8us

/// the global crypto-random pool for uniform and therefore cryptographically
/// secure random values
let cryptRandom = RandomNumberGenerator.Create()

/// Fills the passed array with random bytes
let randomize (bytes : byte []) =
  cryptRandom.GetBytes bytes
  bytes

/// Generates a string key from the available characters with the given key size.
let generateKey (keyLength : uint16) =
  Array.zeroCreate<byte> (int keyLength) |> randomize

let generateStdKey () =
  generateKey KeyLength

let generateIV (ivLength : uint16) =
  Array.zeroCreate<byte> (int ivLength) |> randomize

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
#if NETSTANDARD1_5
  let aes = Aes.Create()
#else
  let aes = new AesManaged()
#endif
  aes.KeySize   <- int KeySize
  aes.BlockSize <- int BlockSize
  aes.Mode      <- CipherMode.CBC
  aes.Padding   <- PaddingMode.PKCS7
  aes.IV        <- iv
  aes.Key       <- key
  aes

let secretbox (key : byte []) (msg : byte []) =
  if key.Length <> int KeyLength then
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
      let bytes = msg |> Compression.gzipEncode
      crypto.Write (bytes, 0, bytes.Length)
      crypto.FlushFinalBlock()
      cipher.ToArray()

    use cipherText = new MemoryStream()

    let bw  = new BinaryWriter(cipherText)
    bw.Write iv
    bw.Write (mkCipherText msg key iv)
    bw.Flush ()

    let hmac = hmacOfBytes key (cipherText.ToArray())
    bw.Write hmac
    bw.Dispose()

    Choice1Of2 (cipherText.ToArray())

let secretboxOfText (key : byte []) (msg : string) =
  secretbox key (msg |> UTF8.bytes)

let secretboxOpen (key : byte []) (cipherText : byte []) =
  let hmacCalc = hmacAtOffset key 0 (cipherText.Length - int HMACLength) cipherText
  let hmacGiven = Array.zeroCreate<byte> (int HMACLength)
  Array.blit cipherText (cipherText.Length - int HMACLength) // from
             hmacGiven  0                                    // to
             (int HMACLength)                                // # bytes for hmac

  if cipherText.Length < int (HMACLength + IVLength) then
    Choice2Of2 (
      TruncatedMessage (
        sprintf "cipher text length was %d but expected >= %d"
                cipherText.Length (HMACLength + IVLength)))
  elif not (Bytes.equalsConstantTime hmacCalc hmacGiven) then
    Choice2Of2 (AlteredOrCorruptMessage "calculated HMAC does not match expected/given")
  else
    let iv = Array.zeroCreate<byte> (int IVLength)
    Array.blit cipherText 0
               iv 0
               (int IVLength)
    use aes     = secretboxInit key iv
    use denc    = aes.CreateDecryptor(key, iv)
    use plain   = new MemoryStream()
    use crypto  = new CryptoStream(plain, denc, CryptoStreamMode.Write)
    crypto.Write(cipherText, int IVLength, cipherText.Length - int IVLength - int HMACLength)
    crypto.FlushFinalBlock()
    Choice1Of2 (plain.ToArray() |> Compression.gzipDecode)

let secretboxOpenAsString keyText cipherText =
  secretboxOpen keyText cipherText |> Choice.map UTF8.toString
