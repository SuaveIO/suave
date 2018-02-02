module internal Suave.Native

open System
open System.Runtime.InteropServices

let SOL_SOCKET_OSX = 0xffff
let SO_REUSEADDR_OSX = 0x0004
let SOL_SOCKET_LINUX = 0x0001
let SO_REUSEADDR_LINUX = 0x0002

[<DllImport("libc", SetLastError = true)>]
extern int setsockopt(IntPtr socket, int level, int option_name, IntPtr option_value, uint32 option_len)