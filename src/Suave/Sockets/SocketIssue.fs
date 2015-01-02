namespace Suave.Sockets

open System.Net.Sockets

exception SocketIssue of SocketError with
  override this.ToString() =
    string this.Data0