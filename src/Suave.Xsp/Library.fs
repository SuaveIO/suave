namespace Suave

open System
open System.IO
open System.Web
open System.Web.Hosting
open System.Text

open Suave
open Suave.Http
open Suave.Utils

open Suave.Sockets

/// The Xsp module allows to host ASP.NET applications in Suave.
/// NOTE: This implementation is incomplete and experimental.
/// The name Suave.Xsp is inspired by Mono's ASP.NET hosting server https://github.com/mono/xsp
module Xsp =

  /// This class acts as broker between application domains and allows the ASP.NET host
  /// to write to the underliying socket connection that lives in the program application domain.
  type Connection(transport : ITransport) =
    /// Inheriting from MarshalByRefObject enables access to objects across application domain boundaries.
    inherit MarshalByRefObject()
    member this.write bs =
      let res = Async.RunSynchronously <| transport.write bs
      match res with
      | Choice1Of2 x -> ()
      | _ ->
        failwith "write failed."
    override this.InitializeLifetimeService() = null

  let maxChunkLength = 60 * 1024

  open System.IO

  /// This class implements the methods used by the ASP.NET runtime to process HTTP requests
  type SuaveWorkerRequest(homeDirectory : string, request : HttpRequest, connection: Connection) =
    inherit SimpleWorkerRequest(request.url.AbsolutePath, request.rawQuery, null)
    let page = request.url.AbsolutePath
    let httpVerb = request.``method``.ToString()
    let postData = request.rawForm
    member this.Write (bytes, len) =
      connection.write (ByteSegment(bytes,0,len))
    override this.SendStatus(statusCode : int, statusDescription : string) =
      let bytes = UTF8.bytes <| String.concat " " [ "HTTP/1.0"; statusCode.ToString(); statusDescription; "\r\n" ]
      this.Write(bytes,bytes.Length)
    override this.SendKnownResponseHeader(index:int, value : string) =
      let headerName = HttpWorkerRequest.GetKnownResponseHeaderName(index)
      let bytes = UTF8.bytes <| String.concat "" [ headerName; ": "; value; "\r\n" ]
      this.Write(bytes,bytes.Length)
    override this.SendUnknownResponseHeader(name: string, value : string) =
      let bytes = UTF8.bytes <| String.concat "" [ name; ": "; value; "\r\n" ]
      this.Write(bytes,bytes.Length)
    override this.SendResponseFromMemory(data : byte [], length : int) =
      // NOTE: on Mono HttpRuntime sends the connection close header and the content-length
      this.SendUnknownResponseHeader("Connection","close")
      let bytes = UTF8.bytes  "\r\n"
      this.Write(bytes, bytes.Length)
      this.Write(data, length)
    override this.GetHttpVerbName() = httpVerb
    override this.GetKnownRequestHeader(index : int) : string =
      match index with
      | HttpWorkerRequest.HeaderContentLength ->
        if postData <> null then postData.Length.ToString() else "0"
      | HttpWorkerRequest.HeaderContentType ->
        match request.header "content-type" with
        | Choice1Of2 v -> v
        | Choice2Of2 _ -> base.GetKnownRequestHeader(index)
      | _ -> base.GetKnownRequestHeader(index)
    override this.GetPreloadedEntityBody() = postData
    override this.GetAppPath() = "/"
    override this.GetAppPathTranslated() = homeDirectory
    override this.GetFilePath() = page
    override this.GetFilePathTranslated() = this.MapPath(page)
    override this.MapPath(path : string) =
      let separator = Path.DirectorySeparatorChar
      let path =
        if separator = '\\' then path.Replace('/','\\') else path
      Path.Combine(homeDirectory, path.TrimStart(separator))
    member this.SendResponseFromFileStream(f : FileStream, offset : int64, length : int64) =
      let fileSize = f.Length;
      this.SendUnknownResponseHeader("Content-length",fileSize.ToString())
      let length = if (length = -1L) then fileSize - offset else length
      if not (length = 0L || offset < 0L || length > fileSize - offset) then
        if (offset > 0L) then
          f.Seek(offset, SeekOrigin.Begin) |> ignore
        if (length <= (int64(maxChunkLength))) then
          let fileBytes = Array.zeroCreate (int(length))
          let bytesRead = f.Read(fileBytes, 0, (int)length)
          this.SendResponseFromMemory(fileBytes, bytesRead)
        else
          let chunk = Array.zeroCreate (64 * 1024)
          let rec loop bytesRemaining =
            if (bytesRemaining > 0) then
              let bytesToRead = if (bytesRemaining < maxChunkLength) then bytesRemaining else maxChunkLength
              let bytesRead = f.Read(chunk, 0, bytesToRead);
              this.SendResponseFromMemory(chunk, bytesRead);
              loop (bytesRemaining - bytesRead)
          loop (int(length))
    override this.SendResponseFromFile( filename: string, offset : int64,  length : int64) =
      if (length > 0L) then
        use  f = new FileStream(filename, FileMode.Open, FileAccess.Read, FileShare.Read)
        this.SendResponseFromFileStream(f, offset, length)

  /// This class will host the execution of HTTP requests in a new application domain
  type SuaveHost() =
    /// Inheriting from MarshalByRefObject enables access to objects across application domain boundaries.
    inherit MarshalByRefObject()
    let mutable rootDir = Path.DirectorySeparatorChar.ToString()
    member this.ProcessRequest(request : HttpRequest, connection: Connection) =
      let worker = new SuaveWorkerRequest(rootDir, request, connection)
      HttpRuntime.ProcessRequest(worker)
    interface IRegisteredObject with
      member this.Stop(b : bool) = ()
    member this.RootDir
      with set(v) = rootDir <- v
    override this.InitializeLifetimeService() = null

  open System.Reflection
  open System.Globalization

  let private createWorkerAppDomainWithHost (hostType : Type) (virtualPath : string ) (physicalPath : string ) : obj =
    // this creates worker app domain in a way that host doesn't need to be in GAC or bin
    // using BuildManagerHost via private reflection
    let uniqueAppString = String.Concat(virtualPath, physicalPath).ToLowerInvariant()
    let appId = (uniqueAppString.GetHashCode()).ToString("x", CultureInfo.InvariantCulture)

    // create BuildManagerHost in the worker app domain
    let appManager = ApplicationManager.GetApplicationManager()
    let buildManagerHostType = typeof<System.Web.HttpRuntime>.Assembly.GetType("System.Web.Compilation.BuildManagerHost")
    let buildManagerHost = appManager.CreateObject(appId, buildManagerHostType, virtualPath, physicalPath, false)

    let args = [| hostType.Assembly.FullName :> obj; hostType.Assembly.Location :> obj|]

    // call BuildManagerHost.RegisterAssembly to make Host type loadable in the worker app domain
    buildManagerHostType.InvokeMember(
      "RegisterAssembly",
      BindingFlags.Instance ||| BindingFlags.InvokeMethod ||| BindingFlags.NonPublic,
      null,
      buildManagerHost,
      args) |> ignore

    // create Host in the worker app domain
    appManager.CreateObject(appId, hostType, virtualPath, physicalPath, false) :> obj

  let private createApplicationHost (hostType : Type, virtualDir : string, physicalDir : string) =
    let physicalDir = if physicalDir.EndsWith("\\") then physicalDir else physicalDir + "\\"
    let aspDir = HttpRuntime.AspInstallDirectory
    let domainId = DateTime.Now.ToString(Globalization.DateTimeFormatInfo.InvariantInfo).GetHashCode().ToString("x")
    let appName = (virtualDir + physicalDir).GetHashCode().ToString("x")
    let setup = new AppDomainSetup()
    setup.ApplicationName <- appName
    let ad = AppDomain.CreateDomain(domainId, null, setup)
    ad.SetData(".appDomain", "*")
    ad.SetData(".appPath", physicalDir)
    ad.SetData(".appVPath", virtualDir)
    ad.SetData(".domainId", domainId)
    ad.SetData(".hostingVirtualPath", virtualDir)
    ad.SetData(".hostingInstallDir", aspDir)
    let oh = ad.CreateInstance(hostType.Module.Assembly.FullName, hostType.FullName)
    oh.Unwrap()

  /// Creates and configures an application domain for hosting ASP.NET.
  let createApplication directory =
    // NOTE: using an application host requires deploying the dll into the bin directory of the ASPX application
    // or registering Suave.Xsp in the GAC
    let binDir = Path.Combine(directory,"bin")
    if not(Directory.Exists(binDir)) then
      Directory.CreateDirectory(binDir) |> ignore
    let asmDir = Directory.GetCurrentDirectory()
    File.Copy(Path.Combine(asmDir, "Suave.dll"), Path.Combine(binDir, "Suave.dll"), true)
    File.Copy(Path.Combine(asmDir, "Suave.Xsp.dll"), Path.Combine(binDir, "Suave.Xsp.dll"), true)

    // Three ways of creating the application domain, we have experimented with these and settled on the last option.

    // let appHost = ApplicationHost.CreateApplicationHost(typeof<SuaveHost>, "/", directory) :?> SuaveHost
    // let appHost = createApplicationHost(typeof<SuaveHost>, "/", directory) :?> SuaveHost

    let appHost =  (createWorkerAppDomainWithHost typeof<SuaveHost>  "/"  directory ) :?> SuaveHost

    appHost.RootDir <- directory
    appHost

  /// Run an ASP.NET application
  let run (appHost : SuaveHost) : WebPart = fun ctx ->
    async {
      do appHost.ProcessRequest( ctx.request, new Connection(ctx.connection.transport))
      return
        { ctx with
            response = { ctx.response with content = NullContent; writePreamble = false }
            request  = { ctx.request  with headers = [ "connection","close"] }} |> Some }
