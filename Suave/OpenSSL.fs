module Suave.OpenSSL

open System
open System.IO
open System.Net
open System.Net.Sockets
open System.Text

open OpenSSL.X509
open OpenSSL.Core
open OpenSSL.SSL

open Native
open Socket

let SSL_CTX_set_mode( ctx : IntPtr,  op : int) = SSL_CTX_ctrl(ctx, SSL_CTRL_MODE, op, IntPtr.Zero)
let SSL_CTX_set_options( ctx : IntPtr, op : int) = SSL_CTX_ctrl(ctx, SSL_CTRL_OPTIONS, op, IntPtr.Zero)
let SSL_CTX_get_mode(ctx : IntPtr) = SSL_CTX_ctrl(ctx, SSL_CTRL_OPTIONS, 0, IntPtr.Zero)
let SSL_CTX_get_options(ctx : IntPtr) = SSL_CTX_ctrl(ctx, SSL_CTRL_OPTIONS, 0, IntPtr.Zero)

let buildCipherString (fipsMode:bool) (sslProtocols: SslProtocols) (sslStrength:SslStrength)= 

  let  str =  if (fipsMode || ((sslStrength &&& SslStrength.High) = SslStrength.High)) then "HIGH" else ""

  let str = 
    if (fipsMode || ((sslStrength &&& SslStrength.Medium) = SslStrength.Medium)) then
      if (String.IsNullOrEmpty(str)) then "MEDIUM" else str + ":MEDIUM"
    else str

  let str =
    if (not(fipsMode) && ((sslStrength &&& SslStrength.Low) = SslStrength.Low)) then
      if (String.IsNullOrEmpty(str)) then "LOW" else str + ":LOW"
    else str

  let str = 
    if ((sslProtocols = SslProtocols.Default) || (sslProtocols = SslProtocols.Tls) || (sslProtocols = SslProtocols.Ssl3))  then
      if (String.IsNullOrEmpty(str)) then "!SSLv2" else str + ":!SSLv2"
    else str

  let str = if (fipsMode) then str + ":AES:3DES:SHA:!DES:!MD5:!IDEA:!RC2:!RC4" else str

  // Now format the return string
  String.Format("{0}:!ADH:!aNULL:!eNULL:@STRENGTH", str)

let create_ssl_server_context (cert : X509Certificate) (clientCertificateRequired : bool) (chain : X509Chain) (protocols : SslProtocols) (strenght : SslStrength) (checkCertificateRevocation : bool) =
  let p = SSLv23_server_method()
  let context = SSL_CTX_new(p)

  SSL_CTX_set_mode (context,SSL_MODE_AUTO_RETRY) |> ignore
  SSL_CTX_set_options (context,SSL_OP_ALL) |> ignore

  SSL_CTX_set_verify (context,SSL_VERIFY_NONE,null)
  SSL_CTX_set_verify_depth (context,10)

  let cipherList = buildCipherString false protocols strenght

  SSL_CTX_set_cipher_list (context, cipherList) |> ignore
  SSL_CTX_use_certificate(context, cert.Handle) |> ignore
  SSL_CTX_use_PrivateKey(context, cert.PrivateKey.Handle) |> ignore
  let sid_ctx = Encoding.ASCII.GetBytes(AppDomain.CurrentDomain.FriendlyName)
  SSL_CTX_set_session_id_context(context, sid_ctx, (uint32)sid_ctx.Length) |> ignore

  context

let SSL3_RT_HEADER_LENGTH = 5
let SSL3_RT_MAX_PLAIN_LENGTH = 16384
let SSL3_RT_MAX_COMPRESSED_LENGTH = (1024 + SSL3_RT_MAX_PLAIN_LENGTH)
let SSL3_RT_MAX_ENCRYPTED_LENGTH = (1024 + SSL3_RT_MAX_COMPRESSED_LENGTH)
let SSL3_RT_MAX_PACKET_SIZE = (SSL3_RT_MAX_ENCRYPTED_LENGTH + SSL3_RT_HEADER_LENGTH)

let BIO_CTRL_SET_CLOSE = 9

let BIO_set_close bp arg = 
  BIO_ctrl(bp, BIO_CTRL_SET_CLOSE, arg, IntPtr.Zero)

open System.Security.Cryptography

let init_open_ssl _ =
  SSL_library_init() |> ignore
  ERR_load_crypto_strings()
  SSL_load_error_strings()
  OPENSSL_add_all_algorithms_noconf()

  let seed = Array.zeroCreate 128
  Globals.crypt_random.GetBytes(seed)
  RAND_seed(seed, seed.Length)

open System.Runtime.InteropServices

let authenticate_as_server (cert : X509Certificate) =
  
  let ssl_context = create_ssl_server_context cert false null SslProtocols.Tls SslStrength.High true
  
  // Initialze read/write bios
  let a = BIO_s_mem()
  let b = BIO_s_mem()
  let read_bio = BIO_new(a)
  let write_bio = BIO_new(b)
  
  let ssl = SSL_new(ssl_context)
  
  // Set the read/write bio's into the the Ssl object
  SSL_set_bio (ssl, read_bio, write_bio)

  //close 1 no-close 0
  BIO_set_close read_bio 1 |> ignore
  BIO_set_close write_bio 1 |> ignore

    // Set the Ssl object into server mode
  SSL_set_accept_state ssl

  ssl,read_bio,write_bio

open Microsoft.FSharp.NativeInterop

let read_to_bio  (con : Connection) read_bio ssl = async {
  let bytes_pending = BIO_ctrl_pending read_bio
  if bytes_pending = 0u then 
    let a = con.get_buffer ()
    let! bytes_read = con.read a
    let buff = Array.zeroCreate bytes_read
    Array.blit a.Array a.Offset buff 0 bytes_read
    con.free_buffer a
    BIO_write(read_bio, buff, bytes_read) |> ignore
  }

let write_from_bio  (con : Connection) write_bio = async {
  let bytes_pending = BIO_ctrl_pending write_bio

  if bytes_pending > 0u  then 
    let encrypted_buff = Array.zeroCreate SSL3_RT_MAX_PACKET_SIZE
    let len = BIO_read(write_bio,encrypted_buff,encrypted_buff.Length)
    do! con.write  (new ArraySegment<_>(encrypted_buff,0,len))
  return ()
  }

let rec accept (logger : Log.Logger) conn (ssl, read_bio, write_bio) = async{
  let ret = SSL_accept ssl
  if(ret < 0) then 

    let bytes_pending = BIO_ctrl_pending write_bio
    
    if bytes_pending > 0u  then do! write_from_bio conn write_bio
    
    let error = SSL_get_error (ssl, ret)
    match error with 
    | x when x = SSL_ERROR_WANT_READ -> 
      do! read_to_bio conn read_bio ssl
      return! accept logger conn (ssl, read_bio, write_bio)
    | x when x = SSL_ERROR_WANT_WRITE -> 
      do! read_to_bio conn read_bio ssl
      return! accept logger conn (ssl, read_bio, write_bio)
    | d -> "OpenSSL error accepting socket" |> Log.interne logger "OpenSSL.accept" (new Exception(sprintf "error code %d" d))
  
  return ()
  }

let ssl_receive (con : Connection) (context, read_bio, write_bio) (bu: B) = async {

  let write_bytes_pending = BIO_ctrl_pending write_bio
  if write_bytes_pending > 0u  then do! write_from_bio con write_bio

  //we need to check if there is data in the read bio before asking for more to the socket
  let bytes_pending = BIO_ctrl_pending read_bio
  if bytes_pending = 0u then 
    let a = con.get_buffer ()
    let! bytes_read = con.read a

    let buff = Array.zeroCreate bytes_read
    Array.blit a.Array a.Offset buff 0 bytes_read
    
    //Copy encrypted data into the SSL read_bio
    con.free_buffer a
    BIO_write(read_bio, buff, bytes_read) |> ignore

  let decrypted_bytes_read = 
    let bytes_pending = BIO_ctrl_pending read_bio

    if bytes_pending > 0ul then
      let buff = Array.zeroCreate SSL3_RT_MAX_PACKET_SIZE
      //SSL_read wants a 0 based array
      let decrypted_bytes_read = SSL_read (context, buff, SSL3_RT_MAX_PACKET_SIZE)
      if decrypted_bytes_read < 0  then 
        let error = SSL_get_error (context, decrypted_bytes_read)
        failwith "SSL_get_error <- %d" error
      else
        //copy them to buf
        Array.blit buff 0 bu.Array bu.Offset decrypted_bytes_read
        decrypted_bytes_read
    else 0

  return decrypted_bytes_read
  }

let ssl_send (con : Connection)  (context, _ , write_bio)  (buf: ArraySegment<_>)= async {

  //SSL_write wants a 0 based array
  let e = SSL_write (context, Array.sub buf.Array buf.Offset buf.Count, buf.Count) 
  //let bytes_pending = BIO_ctrl_pending write_bio
  let encrypted_buff = Array.zeroCreate SSL3_RT_MAX_PACKET_SIZE
  let len = BIO_read(write_bio,encrypted_buff,encrypted_buff.Length)
  return! con.write  (new ArraySegment<_>(encrypted_buff,0,len))
  }
