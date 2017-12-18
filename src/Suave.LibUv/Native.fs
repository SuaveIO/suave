module Suave.LibUv.Native

#nowarn "9"

open System
open System.Runtime.InteropServices

let UV_RUN_DEFAULT = 0
let UV_RUN_ONCE = 1
let UV_RUN_NOWAIT = 2

[<StructLayout(LayoutKind.Sequential)>]
type sockaddr =
  struct
    val mutable sin_family : int16
    val mutable sin_port : uint16
  end

type sockaddr_in =
  struct
    val mutable a : int64
    val mutable b : int64
    val mutable c : int64
    val mutable d : int64
  end

[<StructLayout(LayoutKind.Sequential, Size=28)>]
type sockaddr_in6 =
  struct
    val mutable a : int
    val mutable b : int
    val mutable c : int
    val mutable d : int
    val mutable e : int
    val mutable f : int
    val mutable g : int
  end

[<StructLayout(LayoutKind.Sequential)>]
type uv_buf_t =
  struct
    val mutable ``base`` : IntPtr // pointer to bytes
    val mutable len      : IntPtr
  end

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type uv_connection_cb = delegate of IntPtr * int -> unit

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type uv_alloc_cb = delegate of IntPtr * int * [<Out>] buf:byref<uv_buf_t> -> unit

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type uv_read_cb  = delegate of IntPtr * int * byref<uv_buf_t> -> unit

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type uv_write_cb  = delegate of IntPtr * int -> unit

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type uv_close_cb  = delegate of IntPtr -> unit

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type uv_handle_cb  = delegate of IntPtr -> unit

[<UnmanagedFunctionPointer(CallingConvention.Cdecl)>]
type uv_walk_cb  = delegate of IntPtr * IntPtr -> unit

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_tcp_init(IntPtr loop, IntPtr handle)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_tcp_nodelay(IntPtr handle, int enable)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_ip4_addr(string ip, int port, [<Out>] sockaddr_in& address)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_ip6_addr(string ip, int port, [<Out>] sockaddr_in6& address)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_tcp_bind(IntPtr handle, sockaddr_in& sockaddr, int flags)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl, EntryPoint = "uv_tcp_bind")>]
extern int uv_tcp_bind6(IntPtr handle, sockaddr_in6&  sockaddr, uint32 flags)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_listen(IntPtr stream, int backlog, uv_connection_cb callback)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_accept(IntPtr server, IntPtr client)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_read_start(IntPtr stream, uv_alloc_cb alloc_callback, uv_read_cb read_callback)

[<DllImport ("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_read_stop(IntPtr stream)

/// Loops

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern IntPtr uv_default_loop()

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_loop_init(IntPtr handle)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_loop_close(IntPtr ptr)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_loop_size()

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern void uv_stop(IntPtr loop)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_run(IntPtr loop, int mode)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern void uv_update_time(IntPtr loop)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern uint64 uv_now(IntPtr loop)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern sbyte *uv_strerror(int systemErrorCode)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern sbyte *uv_err_name(int systemErrorCode)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern void uv_close(IntPtr handle, uv_close_cb cb)

[<DllImport("libuv", EntryPoint = "uv_write", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_write(IntPtr req, IntPtr handle, uv_buf_t[] bufs, int bufcnt, uv_write_cb write_callback)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_idle_init(IntPtr loop, IntPtr idle)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_idle_start(IntPtr idle, uv_handle_cb callback)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_idle_stop(IntPtr idle)

type uv_handle_type =
  | UV_UNKNOWN_HANDLE = 0
  | UV_ASYNC          = 1
  | UV_CHECK          = 2
  | UV_FS_EVENT       = 3
  | UV_FS_POLL        = 4
  | UV_HANDLE         = 5
  | UV_IDLE           = 6
  | UV_NAMED_PIPE     = 7
  | UV_POLL           = 8
  | UV_PREPARE        = 9
  | UV_PROCESS        = 10
  | UV_STREAM         = 11
  | UV_TCP            = 12
  | UV_TIMER          = 13
  | UV_TTY            = 14
  | UV_UDP            = 15
  | UV_SIGNAL         = 16
  | UV_FILE           = 17
  | UV_HANDLE_TYPE_PRIVATE = 18
  | UV_HANDLE_TYPE_MAX     = 19

type uv_request_type =
  | UV_UNKNOWN_REQ = 0
  | UV_REQ         = 1
  | UV_CONNECT     = 2
  | UV_WRITE       = 3
  | UV_SHUTDOWN    = 4
  | UV_UDP_SEND    = 5
  | UV_FS          = 6
  | UV_WORK        = 7
  | UV_GETADDRINFO = 8
  | UV_GETNAMEINFO = 9
  | UV_REQ_TYPE_PRIVATE = 10
  | UV_REQ_TYPE_MAX     = 11

let UV_EOF = -4095
let UV_ECONNRESET = -4077

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_handle_size(uv_handle_type t)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_req_size(uv_request_type t)

[<DllImport("libuv", CallingConvention=CallingConvention.Cdecl)>]
extern int uv_async_init(IntPtr loop, IntPtr handle, uv_handle_cb callback)

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern int uv_async_send(IntPtr handle);

[<DllImport("libuv", CallingConvention = CallingConvention.Cdecl)>]
extern void uv_walk(IntPtr loop, uv_walk_cb cb, IntPtr arg);
