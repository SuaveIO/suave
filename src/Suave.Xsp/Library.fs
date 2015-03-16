namespace Suave

open System
open System.IO
open System.Web
open System.Web.Hosting
open System.Text

open Suave
open Suave.Types
open Suave.Types.Codes
open Suave.Http
open Suave.Utils

open Suave.Sockets

module Xsp =

  type SuaveWorkerRequest(homeDirectory : string, page : string, query : string) =
    inherit SimpleWorkerRequest("/", homeDirectory, page, query, null)
    let mutable _data : byte [] = Array.empty
    let mutable _statusCode : int = 0
    override this.SendResponseFromMemory(data : byte [], lenght : int) =
      _data <- data
    override this.SendStatus(statusCode : int, satusDescription) =
      _statusCode <- statusCode
    override this.GetAppPath() = "/"
    override this.GetAppPathTranslated() = homeDirectory
    member this.Data
       with get() = _data
    member this.StatusCode
       with get() = _statusCode
    override this.MapPath(path : string) =
        Path.Combine(homeDirectory, path.TrimStart([| '/'|]))

  type ProcessRequestResult =
      { data : byte[]; status : int }

  type SuaveHost() =
    inherit MarshalByRefObject()
    member this.ProcessRequest(homeDirectory : string, page : string, query : string) =
      let worker = new SuaveWorkerRequest(homeDirectory, page, query)
      HttpRuntime.ProcessRequest(worker)
      { data = worker.Data; status = worker.StatusCode }

  let createApplication directory =
    // NOTE: using an application host requires deploying the dll into the bin directory of the ASPX application 
    // or registering Suave.Xsp in the GAC
    let binDir = Path.Combine(directory,"bin")
    if not(Directory.Exists(binDir)) then
      Directory.CreateDirectory(binDir) |> ignore
    File.Copy(Directory.GetCurrentDirectory() + "/Suave.dll",binDir + "/Suave.dll", true)
    File.Copy(Directory.GetCurrentDirectory() + "/Suave.Xsp.dll",binDir + "/Suave.Xsp.dll", true)
    ApplicationHost.CreateApplicationHost(typeof<SuaveHost>, "/", directory)
    :?> SuaveHost

  let run (appHost : SuaveHost) : WebPart = fun ctx ->
    async {
      let page = ctx.request.url.AbsolutePath.TrimStart([| '/'|])
      let result = appHost.ProcessRequest(ctx.runtime.homeDirectory, page, ctx.request.rawQuery)
      return 
        { ctx with 
            response = 
            { status = HttpCode.TryParse(result.status) |> Option.get; 
              headers = []; 
              content = Bytes result.data }}|> Some
      }