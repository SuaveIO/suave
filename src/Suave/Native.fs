module internal Suave.Native

open System
open System.Runtime.InteropServices

let SOL_SOCKET_OSX = 0xffff
let SO_REUSEADDR_OSX = 0x0004
// macOS / BSD: SO_REUSEPORT allows multiple sockets on the same address+port,
// and the kernel load-balances accepts across them.
let SO_REUSEPORT_OSX = 0x0200

let SOL_SOCKET_LINUX = 0x0001
let SO_REUSEADDR_LINUX = 0x0002
// Linux 3.9+: SO_REUSEPORT enables a kernel-level load-balanced accept set.
let SO_REUSEPORT_LINUX = 15

[<DllImport("libc", SetLastError = true)>]
extern int setsockopt(IntPtr socket, int level, int option_name, IntPtr option_value, uint32 option_len)