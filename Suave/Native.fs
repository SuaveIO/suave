module Native

open System
open System.Runtime.InteropServices

let SSL_ERROR_NONE = 0
let SSL_ERROR_SSL = 1
let SSL_ERROR_WANT_READ = 2
let SSL_ERROR_WANT_WRITE = 3
let SSL_ERROR_WANT_X509_LOOKUP = 4
let SSL_ERROR_SYSCALL = 5 // look at error stack/return value/errno 
let SSL_ERROR_ZERO_RETURN = 6
let SSL_ERROR_WANT_CONNECT = 7
let SSL_ERROR_WANT_ACCEPT = 8

[<DllImport( "ssleay32")>]
extern void SSL_load_error_strings()

[<DllImport( "ssleay32")>]
extern int SSL_library_init()

[<DllImport( "ssleay32")>]
extern void ERR_free_strings()

[<DllImport("libeay32")>]
extern IntPtr BIO_new_file(string filename, string mode)

[<DllImport("libeay32")>]
extern IntPtr BIO_new_mem_buf(byte[] buf, int len)

[<DllImport("libeay32", CallingConvention = CallingConvention.Cdecl)>]
extern IntPtr BIO_s_mem()

[<DllImport("libeay32")>]
extern IntPtr BIO_f_md()

[<DllImport("libeay32")>]
extern IntPtr BIO_f_null()

let BIO_C_SET_FD = 104
let BIO_C_SET_MD = 111
let BIO_C_GET_MD = 112
let BIO_C_GET_MD_CTX = 120
let BIO_C_SET_MD_CTX = 148

let BIO_NOCLOSE = 0x00
let BIO_CLOSE = 0x01

[<DllImport("libeay32")>]
extern void ERR_load_crypto_strings()

[<DllImport("libeay32")>]
extern uint32 ERR_get_error()

[<DllImport("libeay32")>]
extern uint32 ERR_error_string_n(uint32 e, byte[] buf, int len)

[<DllImport("libeay32")>]
extern IntPtr ERR_lib_error_string(uint32 e)

[<DllImport("libeay32")>]
extern IntPtr ERR_func_error_string(uint32 e)

[<DllImport("libeay32")>]
extern IntPtr ERR_reason_error_string(uint32 e)

[<DllImport("libeay32")>]
extern void ERR_remove_state(uint32 pid)

[<DllImport("libeay32")>]
extern void ERR_clear_error()

[<DllImport("libeay32")>]
extern void OPENSSL_add_all_algorithms_noconf();

[<DllImport("libeay32")>]
extern void OPENSSL_add_all_algorithms_conf();

[<DllImport("libeay32")>]
extern IntPtr BIO_push(IntPtr bp, IntPtr append)

[<DllImport("libeay32" , CallingConvention = CallingConvention.Cdecl)>]
extern int BIO_ctrl(IntPtr bp, int cmd, int larg, IntPtr parg)

[<DllImport("libeay32")>]
extern int BIO_int_ctrl(IntPtr bp, int cmd, int larg, int parg)

[<DllImport("libeay32", CallingConvention = CallingConvention.Cdecl)>]
extern  IntPtr BIO_new(IntPtr typ)

[<DllImport("libeay32", CallingConvention = CallingConvention.Cdecl)>]
extern  int BIO_read(IntPtr b, byte[] buf, int len)

open Microsoft.FSharp.NativeInterop

type bytebuffer = nativeptr<byte>

[<DllImport("libeay32", CallingConvention = CallingConvention.Cdecl)>]
extern  int BIO_write(IntPtr b, byte[] buf, int len)
//extern  int BIO_write(IntPtr b, bytebuffer buf, int len)

[<DllImport("libeay32")>]
extern  int BIO_puts(IntPtr b, byte[] buf)

[<DllImport("libeay32")>]
extern  int BIO_gets(IntPtr b, byte[] buf, int len)

[<DllImport("libeay32")>]
extern  void BIO_free(IntPtr bio)

[<DllImport("libeay32")>]
extern  uint32 BIO_number_read(IntPtr bio)

[<DllImport("libeay32")>]
extern  uint32 BIO_number_written(IntPtr bio)

[<DllImport("libeay32", CallingConvention = CallingConvention.Cdecl)>]
extern  uint32 BIO_ctrl_pending(IntPtr bio)

[<DllImport("libeay32")>]
extern void RAND_cleanup();

[<DllImport("libeay32", CallingConvention = CallingConvention.Cdecl)>]
extern void RAND_seed(byte[] buf, int len);

[<DllImport("libeay32")>]
extern int RAND_pseudo_bytes(byte[] buf, int len);

[<DllImport("libeay32")>]
extern int RAND_bytes(byte[] buf, int num);

[<DllImport("libeay32")>]
extern void RAND_add(byte[] buf, int num, double entropy);

[<DllImport("libeay32")>]
extern int RAND_load_file(string file, int max_bytes);

[<DllImport("libeay32")>]
extern int RAND_write_file(string file);

[<DllImport("libeay32")>]
extern string RAND_file_name(byte[] buf, uint32 num);

[<DllImport("libeay32")>]
extern int RAND_status();

[<DllImport("libeay32")>]
extern int RAND_query_egd_bytes(string path, byte[] buf, int bytes);

[<DllImport("libeay32")>]
extern int RAND_egd(string path);

[<DllImport("libeay32")>]
extern int RAND_egd_bytes(string path, int bytes);

[<DllImport("libeay32")>]
extern int RAND_poll();

[<DllImport("libeay32")>]
extern int BN_rand(IntPtr rnd, int bits, int top, int bottom);

[<DllImport("libeay32")>]
extern int BN_pseudo_rand(IntPtr rnd, int bits, int top, int bottom);

[<DllImport("libeay32")>]
extern int BN_rand_range(IntPtr rnd, IntPtr range);

[<DllImport("libeay32")>]
extern int BN_pseudo_rand_range(IntPtr rnd, IntPtr range);

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type pem_password_cb = delegate of IntPtr * int * int * IntPtr -> int

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type GeneratorHandler = delegate of int * int * IntPtr -> int

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type ObjectNameHandler = delegate of IntPtr * IntPtr -> unit

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type CRYPTO_locking_callback = delegate of int * int  * string  * int -> unit

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type CRYPTO_id_callback = delegate of unit -> uint32

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type VerifyCertCallback = delegate of int * IntPtr -> int

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type client_cert_cb = delegate of IntPtr * IntPtr * IntPtr -> int

[<DllImport( "ssleay32")>]
extern IntPtr SSLv2_method()

[<DllImport( "ssleay32")>]
extern IntPtr SSLv2_server_method()

[<DllImport( "ssleay32")>]
extern IntPtr SSLv2_client_method()

[<DllImport( "ssleay32")>]
extern IntPtr SSLv3_method()

[<DllImport( "ssleay32")>]
extern IntPtr SSLv3_server_method()

[<DllImport( "ssleay32")>]
extern IntPtr SSLv3_client_method()

[<DllImport( "ssleay32")>]
extern IntPtr SSLv23_method()

[<DllImport( "ssleay32")>]
extern IntPtr SSLv23_server_method()

[<DllImport( "ssleay32")>]
extern IntPtr SSLv23_client_method()

[<DllImport( "ssleay32")>]
extern IntPtr TLSv1_method()

[<DllImport( "ssleay32")>]
extern IntPtr TLSv1_client_method()

[<DllImport( "ssleay32")>]
extern IntPtr TLSv1_server_method()

[<DllImport( "ssleay32")>]
extern IntPtr DTLSv1_method()

[<DllImport( "ssleay32")>]
extern IntPtr DTLSv1_client_method()

[<DllImport( "ssleay32")>]
extern IntPtr DTLSv1_server_method()

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern IntPtr SSL_CTX_new(IntPtr sslMethod)

[<DllImport( "ssleay32")>]
extern void SSL_CTX_free(IntPtr ctx)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_CTX_ctrl(IntPtr ctx, int cmd, int arg, IntPtr parg)

let SSL_CTRL_OPTIONS = 32
let SSL_CTRL_MODE = 33

let SSL_OP_MICROSOFT_SESS_ID_BUG = 0x00000001
let SSL_OP_NETSCAPE_CHALLENGE_BUG = 0x00000002
let SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG = 0x00000008
let SSL_OP_SSLREF2_REUSE_CERT_TYPE_BUG = 0x00000010
let SSL_OP_MICROSOFT_BIG_SSLV3_BUFFER = 0x00000020
let SSL_OP_MSIE_SSLV2_RSA_PADDING = 0x00000040 (* no effect since 0.9.7h and 0.9.8b *)
let SSL_OP_SSLEAY_080_CLIENT_DH_BUG = 0x00000080
let SSL_OP_TLS_D5_BUG = 0x00000100
let SSL_OP_TLS_BLOCK_PADDING_BUG = 0x00000200

(* Disable SSL 3.0/TLS 1.0 CBC vulnerability workaround that was added
	* in OpenSSL 0.9.6d.  Usually (depending on the application protocol)
	* the workaround is not needed.  Unfortunately some broken SSL/TLS
	* implementations cannot handle it at all, which is why we include
	* it in SSL_OP_ALL. *)
let SSL_OP_DONT_INSERT_EMPTY_FRAGMENTS = 0x00000800 (* added in 0.9.6e *)

(* SSL_OP_ALL: various bug workarounds that should be rather harmless.
	*             This used to be 0x000FFFFFL before 0.9.7. *)
let SSL_OP_ALL = (0x00000FFF ^^^ SSL_OP_NETSCAPE_REUSE_CIPHER_CHANGE_BUG)

(* As server, disallow session resumption on renegotiation *)
let SSL_OP_NO_SESSION_RESUMPTION_ON_RENEGOTIATION = 0x00010000
(* If set, always create a new key when using tmp_dh parameters *)
let SSL_OP_SINGLE_DH_USE = 0x00100000
(* Set to always use the tmp_rsa key when doing RSA operations,
	* even when this violates protocol specs *)
let SSL_OP_EPHEMERAL_RSA = 0x00200000
(* Set on servers to choose the cipher according to the server's
	* preferences *)
let SSL_OP_CIPHER_SERVER_PREFERENCE = 0x00400000
(* If set, a server will allow a client to issue a SSLv3.0 version number
	* as latest version supported in the premaster secret, even when TLSv1.0
	* (version 3.1) was announced in the client hello. Normally this is
	* forbidden to prevent version rollback attacks. *)
let SSL_OP_TLS_ROLLBACK_BUG = 0x00800000

let SSL_OP_NO_SSLv2 = 0x01000000
let SSL_OP_NO_SSLv3 = 0x02000000
let SSL_OP_NO_TLSv1 = 0x04000000

(* The next flag deliberately changes the ciphertest, this is a check
	* for the PKCS#1 attack *)
let SSL_OP_PKCS1_CHECK_1 = 0x08000000
let SSL_OP_PKCS1_CHECK_2 = 0x10000000
let SSL_OP_NETSCAPE_CA_DN_BUG = 0x20000000
let SSL_OP_NETSCAPE_DEMO_CIPHER_CHANGE_BUG = 0x40000000


(* Allow SSL_write(..., n) to return r with 0 < r < n (i.e. report success
	* when just a single record has been written): *)
let SSL_MODE_ENABLE_PARTIAL_WRITE = 0x00000001
(* Make it possible to retry SSL_write() with changed buffer location
	* (buffer contents must stay the same!) this is not the default to avoid
	* the misconception that non-blocking SSL_write() behaves like
	* non-blocking write(): *)
let SSL_MODE_ACCEPT_MOVING_WRITE_BUFFER = 0x00000002
(* Never bother the application with retries if the transport
	* is blocking: *)
let SSL_MODE_AUTO_RETRY = 0x00000004
(* Don't attempt to automatically build certificate chain *)
let SSL_MODE_NO_AUTO_CHAIN = 0x00000008

[<DllImport( "ssleay32")>]
extern void SSL_CTX_set_cert_store(IntPtr ctx, IntPtr cert_store)

let SSL_VERIFY_NONE = 0x00
let SSL_VERIFY_PEER = 0x01
let SSL_VERIFY_FAIL_IF_NO_PEER_CERT = 0x02
let SSL_VERIFY_CLIENT_ONCE = 0x04

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern void SSL_CTX_set_verify(IntPtr ctx, int mode, VerifyCertCallback callback)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern void SSL_CTX_set_verify_depth(IntPtr ctx, int depth)

[<DllImport( "ssleay32")>]
extern void SSL_CTX_set_client_CA_list(IntPtr ctx, IntPtr name_list)

[<DllImport( "ssleay32")>]
extern IntPtr SSL_CTX_get_client_CA_list(IntPtr ctx)

[<DllImport( "ssleay32")>]
extern int SSL_CTX_load_verify_locations(IntPtr ctx, string file, string path)

[<DllImport( "ssleay32")>]
extern int SSL_CTX_set_default_verify_paths(IntPtr ctx)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_CTX_set_cipher_list(IntPtr ctx, string cipher_string)

[<DllImport( "ssleay32")>]
extern int SSL_CTX_use_certificate_chain_file(IntPtr ctx, string file)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_CTX_use_certificate(IntPtr ctx, IntPtr cert)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_CTX_use_PrivateKey(IntPtr ctx, IntPtr pkey)

[<DllImport( "ssleay32")>]
extern int SSL_CTX_use_PrivateKey_file(IntPtr ctx, string file, int typ)

[<DllImport( "ssleay32")>]
extern int SSL_CTX_check_private_key(IntPtr ctx)

let SSL_MAX_SID_CTX_LENGTH = 32

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_CTX_set_session_id_context(IntPtr ctx, byte[] sid_ctx, uint32 sid_ctx_len)

[<DllImport( "ssleay32")>]
extern void SSL_CTX_set_default_passwd_cb_userdata(IntPtr ssl, IntPtr data)

[<DllImport( "ssleay32")>]
extern void SSL_CTX_set_default_passwd_cb(IntPtr ssl, pem_password_cb callback)

[<DllImport( "ssleay32")>]
extern void SSL_CTX_set_client_cert_cb(IntPtr ssl_ctx, client_cert_cb callback)

[<DllImport( "ssleay32")>]
extern IntPtr SSL_CIPHER_description(IntPtr ssl_cipher, byte[] buf, int buf_len)

[<DllImport( "ssleay32")>]
extern string SSL_CIPHER_name(IntPtr ssl_cipher)

[<DllImport( "ssleay32")>]
extern int SSL_CIPHER_get_bits(IntPtr ssl_cipher, [<Out>] int alg_bits)

[<DllImport( "ssleay32")>]
extern IntPtr SSL_get_current_cipher(IntPtr ssl)

[<DllImport( "ssleay32")>]
extern int SSL_get_verify_result(IntPtr ssl)

[<DllImport( "ssleay32")>]
extern int SSL_set_verify_result(IntPtr ssl, int v)

[<DllImport( "ssleay32")>]
extern IntPtr SSL_get_peer_certificate(IntPtr ssl)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_get_error(IntPtr ssl, int ret_code)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_accept(IntPtr ssl)

[<DllImport( "ssleay32")>]
extern int SSL_shutdown(IntPtr ssl)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_write(IntPtr ssl, byte[] buf, int len)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_read(IntPtr ssl, byte[] buf, int len)

[<DllImport( "ssleay32")>]
extern int SSL_renegotiate(IntPtr ssl)

[<DllImport( "ssleay32")>]
extern int SSL_set_session_id_context(IntPtr ssl, byte[] sid_ctx, uint32 sid_ctx_len)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern int SSL_do_handshake(IntPtr ssl)

[<DllImport( "ssleay32")>]
extern void SSL_set_connect_state(IntPtr ssl)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern void SSL_set_accept_state(IntPtr ssl)

[<DllImport( "ssleay32")>]
extern int SSL_connect(IntPtr ssl)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern IntPtr SSL_new(IntPtr ctx)

[<DllImport( "ssleay32")>]
extern void SSL_free(IntPtr ssl)

[<DllImport( "ssleay32", CallingConvention = CallingConvention.Cdecl)>]
extern void SSL_set_bio(IntPtr ssl, IntPtr read_bio, IntPtr write_bio)

[<DllImport( "ssleay32")>]
extern int SSL_use_certificate_file(IntPtr ssl, string file, int typ)

[<DllImport( "ssleay32")>]
extern int SSL_use_PrivateKey_file(IntPtr ssl, string file, int typ)

[<DllImport( "ssleay32")>]
extern int SSL_clear(IntPtr ssl)

[<DllImport( "ssleay32")>]
extern IntPtr SSL_load_client_CA_file(string file)

[<DllImport( "ssleay32")>]
extern IntPtr SSL_get_client_CA_list(IntPtr ssl)

[<DllImport( "ssleay32")>]
extern void SSL_set_client_CA_list(IntPtr ssl, IntPtr name_list)

[<DllImport( "ssleay32")>]
extern IntPtr SSL_get_certificate(IntPtr ssl)

[<DllImport( "ssleay32")>]
extern int SSL_use_certificate(IntPtr ssl, IntPtr x509)

[<DllImport( "ssleay32")>]
extern int SSL_use_PrivateKey(IntPtr ssl, IntPtr evp_pkey)