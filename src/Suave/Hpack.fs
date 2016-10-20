namespace Suave

/// HPACK compression format (largely coppied from Haskell's HTTP2 package)
module Hpack =

  open System

  let defaultDynamicTableSize = 4096

  type HeaderName = string

  type HeaderValue = string

  type Header = HeaderName * HeaderValue

  type HeaderList = Header list

  type CompressionAlgorithm = Naive | Static | Linear

  type EncodeStrategy = {
    algorithm : CompressionAlgorithm;
    useHuffman : bool
    }

  let defaultEncodeStrategy = { algorithm = Linear; useHuffman = false }

  type Index = int

  type DecodeError = 
    | IndexOverrun of Index // ^ Index is out of range
    | EosInTheMiddle // ^ Eos appears in the middle of huffman string
    | IllegalEos // ^ Non-eos appears in the end of huffman string
    | TooLongEos // ^ Eos of huffman string is more than 7 bits
    | EmptyEncodedString // ^ Encoded string has no length
    | TooSmallTableSize // ^ A peer set the dynamic table size less than 32
    | TooLargeTableSize // ^ A peer tried to change the dynamic table size over the limit
    | IllegalTableSizeUpdate // ^ Table size update at the non-beginning
    | HeaderBlockTruncated
    | IllegalHeaderName

  type Token = {
    index : int
    shouldIndex : bool // should be indexed in HPACK
    isPseudo : bool
    tokenKey : string  // case-insensitive header key 
    }

  type Entry =  {
    size : int
    token : Token
    headerValue : HeaderValue
    }

  let headerSizeMagicNumber = 32

  let headerSize' (t : Token) (v : HeaderValue) = 
    t.tokenKey.ToLowerInvariant().Length +
    + v.Length
    + headerSizeMagicNumber

  let toEntryToken t v = { size = (headerSize' t v); token = t; headerValue = v }

  type HIndex = SIndex of int | DIndex of int

  type ValueMap = Map<HeaderValue, HIndex>

  type KeyValue = KeyValue of HeaderName * HeaderValue

  type OtherRevIdex = Map<KeyValue, HIndex>

  type DynamicRevIndex = ValueMap array

  type RevIndex = RevIndex of DynamicRevIndex * OtherRevIdex

  open Suave.Huffman

  type ReadBuffer = IO.MemoryStream

  type HuffmanDecoding = IO.MemoryStream -> int -> byte []

  type CodeInfo =
    | EncodeInfo of (RevIndex * (int option))
    | DecodeInfo of (HuffmanDecoding * int)

  type Table = Entry array

  type DynamicTable (codeInfo: CodeInfo, circularTable: Table,offset, numOfEntries, maxNumOfEntries, dynamicTableSize, maxDynamicTableSize) =
    member val codeInfo = codeInfo
    member val circularTable = circularTable
    member val offset = offset with get,set
    member val numOfEntries  = numOfEntries with get,set
    member val maxNumOfEntries  = maxNumOfEntries with get,set
    member val dynamicTableSize = dynamicTableSize with get,set

  let staticHeaderList : HeaderList = [
    (":authority", "")
    (":method", "GET")
    (":method", "POST")
    (":path", "/")
    (":path", "/index.html")
    (":scheme", "http")
    (":scheme", "https")
    (":status", "200")
    (":status", "204")
    (":status", "206")
    (":status", "304")
    (":status", "400")
    (":status", "404")
    (":status", "500")
    ("accept-charset", "")
    ("accept-encoding", "gzip, deflate")
    ("accept-language", "")
    ("accept-ranges", "")
    ("accept", "")
    ("access-control-allow-origin", "")
    ("age", "")
    ("allow", "")
    ("authorization", "")
    ("cache-control", "")
    ("content-disposition", "")
    ("content-encoding", "")
    ("content-language", "")
    ("content-length", "")
    ("content-location", "")
    ("content-range", "")
    ("content-type", "")
    ("cookie", "")
    ("date", "")
    ("etag", "")
    ("expect", "")
    ("expires", "")
    ("from", "")
    ("host", "")
    ("if-match", "")
    ("if-modified-since", "")
    ("if-none-match", "")
    ("if-range", "")
    ("if-unmodified-since", "")
    ("last-modified", "")
    ("link", "")
    ("location", "")
    ("max-forwards", "")
    ("proxy-authenticate", "")
    ("proxy-authorization", "")
    ("range", "")
    ("referer", "")
    ("refresh", "")
    ("retry-after", "")
    ("server", "")
    ("set-cookie", "")
    ("strict-transport-security", "")
    ("transfer-encoding", "")
    ("user-agent", "")
    ("vary", "")
    ("via", "")
    ("www-authenticate", "") ]

  let staticHeaderArray = List.toArray staticHeaderList

  let staticTableSize = staticHeaderArray.Length

  [<AbstractClass; Sealed>]
  type StaticTable() =
    static member get index = staticHeaderArray.[index]

  let toStaticEntry (idx:int) = StaticTable.get idx

  let adj maxN x =
    if maxN = 0 then failwith "Too small table size."
    else
      let ret = (x + maxN) / maxN
      ret

  let headerSize (kv : Header) = (fst kv).Length + (snd kv).Length + headerSizeMagicNumber

  let mkToken index shouldIndex isPseudo tokenKey =
    { index = index; shouldIndex = shouldIndex; isPseudo = isPseudo; tokenKey = tokenKey }

  let tokenAuthority                = mkToken 0  true  true ":authority"
  let tokenMethod                   = mkToken 1  true  true ":method"
  let tokenPath                     = mkToken 2 false  true ":path"
  let tokenScheme                   = mkToken 3  true  true ":scheme"
  let tokenStatus                   = mkToken 4  true  true ":status"
  let tokenAcceptCharset            = mkToken 5  true  false "Accept-Charset"
  let tokenAcceptEncoding           = mkToken 6  true  false "Accept-Encoding"
  let tokenAcceptLanguage           = mkToken 7  true  false "Accept-Language"
  let tokenAcceptRanges             = mkToken 8  true  false "Accept-Ranges"
  let tokenAccept                   = mkToken 9  true  false "Accept"
  let tokenAccessControlAllowOrigin = mkToken 10  true false "Access-Control-Allow-Origin"
  let tokenAge                      = mkToken 11  true false "Age"
  let tokenAllow                    = mkToken 12  true false "Allow"
  let tokenAuthorization            = mkToken 13  true false "Authorization"
  let tokenCacheControl             = mkToken 14  true false "Cache-Control"
  let tokenContentDisposition       = mkToken 15  true false "Content-Disposition"
  let tokenContentEncoding          = mkToken 16  true false "Content-Encoding"
  let tokenContentLanguage          = mkToken 17  true false "Content-Language"
  let tokenContentLength            = mkToken 18 false false "Content-Length"
  let tokenContentLocation          = mkToken 19 false false "Content-Location"
  let tokenContentRange             = mkToken 20  true false "Content-Range"
  let tokenContentType              = mkToken 21  true false "Content-Type"
  let tokenCookie                   = mkToken 22  true false "Cookie"
  let tokenDate                     = mkToken 23  true false "Date"
  let tokenEtag                     = mkToken 24 false false "Etag"
  let tokenExpect                   = mkToken 25  true false "Expect"
  let tokenExpires                  = mkToken 26  true false "Expires"
  let tokenFrom                     = mkToken 27  true false "From"
  let tokenHost                     = mkToken 28  true false "Host"
  let tokenIfMatch                  = mkToken 29  true false "If-Match"
  let tokenIfModifiedSince          = mkToken 30  true false "If-Modified-Since"
  let tokenIfNoneMatch              = mkToken 31  true false "If-None-Match"
  let tokenIfRange                  = mkToken 32  true false "If-Range"
  let tokenIfUnmodifiedSince        = mkToken 33  true false "If-Unmodified-Since"
  let tokenLastModified             = mkToken 34  true false "Last-Modified"
  let tokenLink                     = mkToken 35  true false "Link"
  let tokenLocation                 = mkToken 36  true false "Location"
  let tokenMaxForwards              = mkToken 37  true false "Max-Forwards"
  let tokenProxyAuthenticate        = mkToken 38  true false "Proxy-Authenticate"
  let tokenProxyAuthorization       = mkToken 39  true false "Proxy-Authorization"
  let tokenRange                    = mkToken 40  true false "Range"
  let tokenReferer                  = mkToken 41  true false "Referer"
  let tokenRefresh                  = mkToken 42  true false "Refresh"
  let tokenRetryAfter               = mkToken 43  true false "Retry-After"
  let tokenServer                   = mkToken 44  true false "Server"
  let tokenSetCookie                = mkToken 45 false false "Set-Cookie"
  let tokenStrictTransportSecurity  = mkToken 46  true false "Strict-Transport-Security"
  let tokenTransferEncoding         = mkToken 47  true false "Transfer-Encoding"
  let tokenUserAgent                = mkToken 48  true false "User-Agent"
  let tokenVary                     = mkToken 49  true false "Vary"
  let tokenVia                      = mkToken 50  true false "Via"
  let tokenWwwAuthenticate          = mkToken 51  true false "Www-Authenticate"
  // Not defined in the static table.
  let tokenConnection               = mkToken 52 false false "Connection"
  // Not defined in the static table.
  let tokenTE                       = mkToken 53 false false "TE"
  // A place holder to hold header keys not defined in the static table.
  let tokenMax                      = mkToken 54  true false "for other tokens"

  let maxStaticTokenIndex = 51
  let maxTokenIndex = 54
  let minTokenIx = 0

  let isStaticToken (token:Token) = token.index <= maxStaticTokenIndex

  let mkTokenMax (str:string) =
    let flag =
      if str.Length = 0 then false
      else
        if str.[0] = ':' then true
        else false
    mkToken maxTokenIndex true flag str

  let toToken (str: HeaderValue) : Token =
    let lc = str.ToLowerInvariant()
    let len = lc.Length
    let last = Convert.ToByte (lc.[ len - 1])
    match len with
    | 2 ->
      if lc = "te" then tokenTE else mkTokenMax lc
    | 3 ->
      match last with
      | 97uy  when lc = "via" -> tokenVia
      | 101uy when lc = "age" -> tokenAge
      | _                     -> mkTokenMax lc
    | 4 -> 
      match last with
      | 101uy when lc = "date" -> tokenDate
      | 103uy when lc = "etag" -> tokenEtag
      | 107uy when lc = "link" -> tokenLink
      | 109uy when lc = "from" -> tokenFrom
      | 116uy when lc = "host" -> tokenHost
      | 121uy when lc = "vary" -> tokenVary
      | _                      -> mkTokenMax lc
    | 5 ->
      match last with
      | 101uy when lc = "range" -> tokenRange
      | 104uy when lc = ":path" -> tokenPath
      | 119uy when lc = "allow" -> tokenAllow
      | _                       -> mkTokenMax lc
    | 6 ->
      match last with
      | 101uy when lc = "cookie" -> tokenCookie
      | 114uy when lc = "server" -> tokenServer
      | 116uy when lc = "expect" -> tokenExpect
      | 116uy when lc = "accept" -> tokenAccept
      | _                        -> mkTokenMax lc
    | 7 ->
      match last with
      | 100uy when lc = ":method" -> tokenMethod
      | 101uy when lc = ":scheme" -> tokenScheme
      | 104uy when lc = "refresh" -> tokenRefresh
      | 114uy when lc = "referer" -> tokenReferer
      | 115uy when lc = "expires" -> tokenExpires
      | 115uy when lc = ":status" -> tokenStatus
      | _                         -> mkTokenMax lc
    | 8 ->
      match last with
      | 101uy when lc = "if-range" -> tokenIfRange
      | 104uy when lc = "if-match" -> tokenIfMatch
      | 110uy when lc = "location" -> tokenLocation
      | _                          -> mkTokenMax lc
    | 10 ->
      match last with
      | 101uy when lc = "set-cookie" -> tokenSetCookie
      | 110uy when lc = "connection" -> tokenConnection
      | 116uy when lc = "user-agent" -> tokenUserAgent
      | 121uy when lc = ":authority" -> tokenAuthority
      | _                            -> mkTokenMax lc
    | 11 ->
      match last with
      | 114uy when lc = "retry-after" -> tokenRetryAfter
      | _                             -> mkTokenMax lc
    | 12 ->
      match last with
      | 101uy when lc = "content-type" -> tokenContentType
      | 115uy when lc = "max-forwards" -> tokenMaxForwards
      | _                              -> mkTokenMax lc
    | 13 ->
      match last with
      | 100uy when lc = "last-modified" -> tokenLastModified
      | 101uy when lc = "content-range" -> tokenContentRange
      | 104uy when lc = "if-none-match" -> tokenIfNoneMatch
      | 108uy when lc = "cache-control" -> tokenCacheControl
      | 110uy when lc = "authorization" -> tokenAuthorization
      | 115uy when lc = "accept-ranges" -> tokenAcceptRanges
      | _                               -> mkTokenMax lc
    | 14 ->
      match last with
      | 104uy when lc = "content-length" -> tokenContentLength
      | 116uy when lc = "accept-charset" -> tokenAcceptCharset
      | _                                -> mkTokenMax lc
    | 15 ->
      match last with
      | 101uy when lc = "accept-language" -> tokenAcceptLanguage
      | 103uy when lc = "accept-encoding" -> tokenAcceptEncoding
      | _                                 -> mkTokenMax lc
    | 16 ->
      match last with
      | 101uy when lc = "content-language" -> tokenContentLanguage
      | 101uy when lc = "www-authenticate" -> tokenWwwAuthenticate
      | 103uy when lc = "content-encoding" -> tokenContentEncoding
      | 110uy when lc = "content-location" -> tokenContentLocation
      | _                                  -> mkTokenMax lc
    | 17 ->
      match last with
      | 101uy when lc = "if-modified-since" -> tokenIfModifiedSince
      | 103uy when lc = "transfer-encoding" -> tokenTransferEncoding
      | _                                   -> mkTokenMax lc
    | 18 ->
      match last with
      | 101uy when lc = "proxy-authenticate" -> tokenProxyAuthenticate
      | _                                    -> mkTokenMax lc
    | 19 ->
      match last with
      | 101uy when lc = "if-unmodified-since" -> tokenIfUnmodifiedSince
      | 110uy when lc = "proxy-authorization" -> tokenProxyAuthorization
      | 110uy when lc = "content-disposition" -> tokenContentDisposition
      | _                                     -> mkTokenMax lc
    | 25 ->
      match last with
      | 121uy when lc = "strict-transport-security" -> tokenStrictTransportSecurity
      | _                                           -> mkTokenMax lc
    | 27 ->
      match last with
      | 110uy when lc = "access-control-allow-origin" -> tokenAccessControlAllowOrigin
      | _                                             -> mkTokenMax lc
    | _  -> mkTokenMax lc

  type TokenHeader = Token * HeaderValue

  type TokenHeaderList = TokenHeader list

  let toEntry (kv:Header) : Entry =
    {size = headerSize kv;token = toToken (fst kv); headerValue = (snd kv)}

  let toDynamicEntry (dyntbl:DynamicTable) (idx:int) =
    let maxN = dyntbl.maxNumOfEntries
    let off = dyntbl.offset
    let dix = adj maxN (idx + off - staticTableSize)
    dyntbl.circularTable.[dix]

  let toIndexedEntry (dyntbl:DynamicTable) (idx:int) =
    if idx < 0 then failwith "Index overrun."
    elif idx <= staticTableSize then toStaticEntry idx |> toEntry
    else toDynamicEntry dyntbl idx

  /// encode index

(*
if I < 2^N - 1, encode I on N bits
   else
       encode (2^N - 1) on N bits
       I = I - (2^N - 1)
       while I >= 128
            encode (I % 128 + 128) on 8 bits
            I = I / 128
       encode I on 8 bits
*)

  let powerArray = [| 1uy; 3uy; 7uy; 15uy; 31uy; 63uy; 127uy; 255uy |]

  open System.IO

  type Setter = byte -> byte

  let encodeInteger (wbuf : MemoryStream) (set : Setter) (n: int) (i: int) =
    let p = powerArray.[n]
    if n < (int p) then
      wbuf.WriteByte (set (byte i))
    else
      wbuf.WriteByte (set  p)
      let rec encode' (j: int) =
        if j < int 128uy then
          wbuf.WriteByte (byte j)
        else
          let q = j >>> 7
          let r = byte (j &&& 0x7f)

          wbuf.WriteByte( r + 128uy)
          encode' q
      encode' (i - (int p))

  open System.Text

  let copyByteString (wbuf : MemoryStream) (str : string) =
    let bytes = Encoding.UTF8.GetBytes(str)
    wbuf.Write(bytes,0,bytes.Length)

  let naiveAlgorithm fe (t:Token) v = fe (t.tokenKey.ToLower()) v

  let staticAlgorithm fa fd fe (t:Token) v = ()

  type StaticEntry = StaticEntry of HIndex * ValueMap option

  let lookupOtherRevIndex (k,v) (ref: OtherRevIdex) fa' fc' =
    match Map.tryFind (KeyValue (k,v)) ref with
    | Some i -> fa' i
    | None   -> fc'

  let toEnt (k : HeaderValue, xs) = 
    let token = (toToken k)
    let m =
      match xs with
      | [] -> failwith "staticRevIndex"
      | ["",i] -> StaticEntry (i, None)
      | (_,i):: _ ->
        let map = Map.ofList xs
        StaticEntry (i,Some map)
    token.index , m

  let span p = 
    let rec loop k = function
    | x :: xs' when p x ->
        let f (ys, zs) = x :: ys, zs
        loop (f >> k) xs'
    | xs -> k ([], xs)
    loop id

  let groupBy eq = 
    let rec loop k = function
    | [] -> k []
    | x :: xs ->
        let (ys, zs) = span (eq x) xs
        let f zss = (x :: ys) :: zss
        loop (f >> k) zs
    loop id

  let staticRevIndex =
    let extract xs = (fst (List.head xs), List.map snd xs)
    let lst = List.map2 (fun (k,v) i -> (k,(v,i))) staticHeaderList (List.map SIndex [1 .. staticHeaderArray.Length])
    let groups = groupBy (fun x y -> fst x = fst y) lst
    let zs = List.map extract groups
    List.map toEnt zs

  let lookupStaticRevIndex (ix:int) (v:HeaderValue) fa' fbd = ()

  let lookupDynamicStaticRevIndex (ix:int) (v:HeaderValue) (drev:DynamicRevIndex) fa' fbd' =
    let map = drev.[ix]
    match Map.tryFind v map with
    | Some i -> fa' i
    | None   -> lookupStaticRevIndex ix v fa' fbd'

  let lookupRevIndex (t:Token) v fa fb fc fd (RevIndex( dyn, oth)) =
    let ent = toEntryToken t v
    if not (isStaticToken t) then
      lookupOtherRevIndex (t.tokenKey.ToLowerInvariant(),v) oth fa (fc (t.tokenKey.ToLowerInvariant()) v ent)
    elif t.shouldIndex then 
      lookupDynamicStaticRevIndex t.index v dyn fa (fb v ent)
    else
      lookupStaticRevIndex t.index v fa (fd v)

  let linearAlgorithm rev fa fb fc fd t v =
    lookupRevIndex t v fa fb fc fd rev

  let encodeString (useHuffman : bool) (str: string) (wbuf : MemoryStream) =
    if useHuffman then
      ()
    else
      let len = str.Length
      encodeInteger wbuf id 7 len
      copyByteString wbuf str

  let set1    x = x ||| (1uy <<< 7)
  let set01   x = x ||| (1uy <<< 6)
  let set001  x = x ||| (1uy <<< 5)
  let set0001 x = x ||| (1uy <<< 4) // unused
  let set0000   = id
  let setH      = set1

  let newName (wbuf : MemoryStream) huff (set : Setter) k v = do
    wbuf.WriteByte (set 0uy)
    encodeString huff k wbuf
    encodeString huff v wbuf

  let index (wbuf : MemoryStream) i =
    encodeInteger wbuf set1 7 i

  let indexedName wbuf huff n set v idx = do
    encodeInteger wbuf set n idx
    encodeString huff v wbuf

  let literalHeaderFieldWithoutIndexingNewName' _ wbuf huff k v =
    newName wbuf huff id k v
 
  let useHuffman  = true
  (*
  let adj max x =
    if max = 0 then failwith "dynamic table size is zero"
    else
      (x + max) % max*)

  let fromHIndexToIndex (dyntbl : DynamicTable) = function
    | SIndex index -> index
    | DIndex index -> (adj dyntbl.maxNumOfEntries (index - dyntbl.offset)) + staticTableSize

  let indexedHeaderField dyntbl wbuf _ hidx =
    fromHIndexToIndex dyntbl hidx |> index wbuf

  let insertEntry (ent : Entry) (dynbtl : DynamicTable) =
    let i = dynbtl.offset
    dynbtl.offset <- adj dynbtl.maxNumOfEntries (dynbtl.offset - 1)
    dynbtl.circularTable.[i] <- ent
    dynbtl.numOfEntries <- dynbtl.numOfEntries + 1
    dynbtl.dynamicTableSize <- dynbtl.dynamicTableSize + ent.size
    // TODO: something is missing here

  let literalHeaderFieldWithIncrementalIndexingIndexedName dyntbl wbuf huff v ent hidx = do
    fromHIndexToIndex dyntbl hidx |> indexedName wbuf huff 6 set01 v
    insertEntry ent dyntbl

  let literalHeaderFieldWithIncrementalIndexingNewName dyntbl wbuf huff k v ent = do
    newName wbuf huff set01 k v
    insertEntry ent dyntbl

  let literalHeaderFieldWithoutIndexingIndexedName dyntbl wbuf huff v hidx =
    fromHIndexToIndex dyntbl hidx |> indexedName wbuf huff 4 set0000 v

  let literalHeaderFieldWithoutIndexingNewName _ wbuf huff k v =
    newName wbuf huff set0000 k v

  let getRevIndex (dynbtl : DynamicTable) : RevIndex =
    let (EncodeInfo (rev,_)) = dynbtl.codeInfo
    rev

  let encodeTokenHeader (wbuf : MemoryStream) size (strategy: EncodeStrategy) (first: bool) (dyntbl:DynamicTable) (hl:TokenHeaderList) =
    let fa = indexedHeaderField dyntbl wbuf useHuffman
    let fb = literalHeaderFieldWithIncrementalIndexingIndexedName dyntbl wbuf useHuffman
    let fc = literalHeaderFieldWithIncrementalIndexingNewName dyntbl wbuf useHuffman
    let fd = literalHeaderFieldWithoutIndexingIndexedName dyntbl wbuf useHuffman
    let fe = literalHeaderFieldWithoutIndexingNewName dyntbl wbuf useHuffman
    let rev = getRevIndex dyntbl
    let algorithm =
      match strategy.algorithm with
      | Naive ->
        naiveAlgorithm (literalHeaderFieldWithoutIndexingNewName' dyntbl wbuf useHuffman)
      | Static ->
        staticAlgorithm fa fd fe
      | Linear ->
        linearAlgorithm rev fa fb fc fd
    [],1

  let encodeHeader' (strategy : EncodeStrategy) (size : int) (dyntbl : DynamicTable) (hl : TokenHeaderList) : byte array =
    let buf = Array.zeroCreate<byte> size
    let (hl',len) = encodeTokenHeader (new MemoryStream(buf)) size strategy true dyntbl hl
    if hl' = [] then Array.sub buf 0 len
    else failwith "buffer overflow"

  // produce HPACK format
  let encodeHeader (strategy : EncodeStrategy) (size : int) (dyntbl : DynamicTable) (hl : HeaderList) : byte array =
    encodeHeader' strategy size dyntbl (List.map (fun (a,b) -> let c = toToken a in (c,b)) hl)

  let isTableSizeUpdate (w: byte) = w &&& (byte 0xe0) = (byte 0x20)

  let mask4 w = w &&& 15uy
  let mask5 w = w &&& 31uy
  let mask6 w = w &&& 63uy

  let isset (x:byte) i = x &&& (1uy <<< i) <> 0uy

  let decode (n:int) (w:byte) (rbuf:  MemoryStream) =
    let rec decode' m j =
      let b = rbuf.ReadByte()
      let j' = j + (b &&& 0x7f)*(pown 2 m)
      let m' = m + 7
      if isset (byte b) 7 then 
        decode' m' j' 
      else j'
    let p = powerArray.[n - 1]
    if w < p then int w
    else decode' 0 (int w)

  let isSuitableSize size (dyntbl : DynamicTable) =
    let (DecodeInfo(_,limiref)) = dyntbl.codeInfo
    size < limiref

  let renewDynamicTable size dyntbl =
    dyntbl

  let tableSizeUpdate (dyntbl : DynamicTable) (w: Byte) (rbuf:  MemoryStream) =
    let w' = mask5 w
    let size = decode 5 w' rbuf
    if isSuitableSize size dyntbl then
      renewDynamicTable size dyntbl
    else
      failwith "TooLargeTableSize"

  let rec chkChange (dyntbl : DynamicTable) (rbuf : MemoryStream)(dec : DynamicTable -> MemoryStream -> HeaderList) : HeaderList =
    let edn = rbuf.Length
    if rbuf.Position <> int64 edn then
      let w = byte (rbuf.ReadByte())
      if isTableSizeUpdate w then
        let dyntbl = tableSizeUpdate dyntbl w rbuf
        chkChange dyntbl rbuf dec
      else
        rbuf.Position <- rbuf.Position - 1L
        dec dyntbl rbuf
    else
      failwith "Header block truncated."

  let decodeHPACK (dyntbl : DynamicTable) (inp: byte array) (dec : DynamicTable -> MemoryStream -> HeaderList) : HeaderList =
    chkChange dyntbl (new MemoryStream(inp)) dec

  let entryTokenHeader (e:Entry) : TokenHeader =
    e.token,e.headerValue

  open Utils.BitOperations

  let indexed (dyntbl : DynamicTable) (w: byte) rbuf : TokenHeader =
    let w' = unset w 7
    // The index value of 0 is not used. It MUST be treated as a decoding error if found in an indexed header field representation.
    assert (w <> 0uy)
    let idx = decode 7 w' rbuf
    toIndexedEntry dyntbl idx |> entryTokenHeader

  let isIndexedName1 w = mask6 w <> 0uy
  let isIndexedName2 w = mask4 w <> 0uy

  let entryToken (e:Entry) = e.token

  let dropHuffman w = unset w 7
  let isHuffman w   = isset w 7

  let extractByteString (rbuf:MemoryStream) (len:int) =
    Array.sub (rbuf.GetBuffer()) 0 len

  let decodeString (huff:bool) (decoder: HuffmanDecoding)  (rbuf:MemoryStream) (len:int) =
    if rbuf.Length <> rbuf.Position then
      if huff then
        decoder rbuf len
      else
        extractByteString rbuf len
    else
      failwith "Header block truncated"

  let huffmanDecoder (dyntbl : DynamicTable) = 
    let (DecodeInfo(dec,_)) = dyntbl.codeInfo
    dec

  let headerStuff (dyntbl : DynamicTable) (rbuf:MemoryStream) =
    let w = byte(rbuf.ReadByte())
    let p = dropHuffman w
    let huff = isHuffman w
    let len = decode 7 p rbuf
    decodeString huff (huffmanDecoder dyntbl) rbuf len

  let inserIndexedName (dyntbl : DynamicTable) (w: byte) rbuf n mask =
    let p = mask w
    let idx = decode n p rbuf
    let t = entryToken(toIndexedEntry dyntbl idx)
    let v = Text.Encoding.UTF8.GetString(headerStuff dyntbl rbuf)
    (t,v)

  let insertNewName (dyntbl : DynamicTable) (rbuf:MemoryStream) =
    let t = toToken (Text.Encoding.UTF8.GetString(headerStuff dyntbl rbuf))
    let v = Text.Encoding.UTF8.GetString(headerStuff dyntbl rbuf)
    (t,v)

  let incrementalIndexing (dyntbl : DynamicTable) (w: byte) rbuf : TokenHeader =
    let tv =
      if isIndexedName1 w then
        inserIndexedName dyntbl w rbuf 6 mask6
      else
        insertNewName dyntbl rbuf
    let e = toEntryToken (fst tv) (snd tv)
    insertEntry e dyntbl
    tv

  let neverIndexing (dyntbl : DynamicTable) (w: byte) rbuf : TokenHeader =
    failwith "neverIndexing: not implemented"

  let withoutIndexing (dyntbl : DynamicTable) (w: byte) rbuf : TokenHeader =
    failwith "withoutIndexing: not implemented"

  let toTokenHeader (dyntbl : DynamicTable) (w: byte) rbuf =
    if   isset w 7  then indexed dyntbl w rbuf
    elif isset w 6  then incrementalIndexing dyntbl w rbuf
    elif isset w 5  then failwith "Illegal table size update"
    elif isset w 4  then neverIndexing dyntbl w rbuf
    else withoutIndexing dyntbl w rbuf

  open System.Collections.Generic

  let decodeSimple (dyntbl : DynamicTable) (rbuf : MemoryStream) : HeaderList = 
    let list = new List<Header>()
    while rbuf.Position <> rbuf.Length do
      let w = byte(rbuf.ReadByte())
      let (token,headerValue) = toTokenHeader dyntbl w rbuf
      list.Add (token.tokenKey,headerValue)
    Seq.toList list

  let decodeHeader (dyntbl : DynamicTable) (inp: byte array) : HeaderList =
    decodeHPACK dyntbl inp decodeSimple
