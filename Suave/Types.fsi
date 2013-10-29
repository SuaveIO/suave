namespace Suave
  module Types = begin
    type HttpResponse =
      class
        new : unit -> HttpResponse
        member Headers : System.Collections.Generic.List<string * string>
        member
          Headers : System.Collections.Generic.List<string * string> with set
      end
    type HttpUpload =
      class
        new : fieldname:string * filename:string * mime_type:string *
              temp_file_name:string -> HttpUpload
        member FieldName : string
        member FileName : string
        member MimeType : string
        member Path : string
      end
    type HttpRequest =
      class
        interface System.IDisposable
        new : unit -> HttpRequest
        member private Dispose : disposing:bool -> unit
        override Finalize : unit -> unit
        member
          Cookies : System.Collections.Generic.Dictionary<string,
                                                          (string * string) []>
        member Files : System.Collections.Generic.List<HttpUpload>
        member Form : System.Collections.Generic.Dictionary<string,string>
        member Headers : System.Collections.Generic.Dictionary<string,string>
        member Method : string
        member Password : string
        member Query : System.Collections.Generic.Dictionary<string,string>
        member RawForm : byte []
        member RawQuery : string
        member RemoteAddress : string
        member Response : HttpResponse
        member SessionId : string
        member Stream : System.IO.Stream
        member Url : string
        member Username : string
        member
          Cookies : System.Collections.Generic.Dictionary<string,
                                                          (string * string) []>
                      with set
        member
          Form : System.Collections.Generic.Dictionary<string,string> with set
        member
          Headers : System.Collections.Generic.Dictionary<string,string>
                      with set
        member Method : string with set
        member Password : string with set
        member
          Query : System.Collections.Generic.Dictionary<string,string> with set
        member RawForm : byte [] with set
        member RawQuery : string with set
        member RemoteAddress : string with set
        member SessionId : string with set
        member Stream : System.IO.Stream with set
        member Url : string with set
        member Username : string with set
      end
    type Protocol =
      | HTTP
      | HTTPS of System.Security.Cryptography.X509Certificates.X509Certificate
      with
        override ToString : unit -> string
        static member
          FromString : scheme:string *
                       ?cert:#System.Security.Cryptography.X509Certificates.X509Certificate ->
                         Protocol
      end
    type HttpBinding =
      {scheme: Protocol;
       ip: System.Net.IPAddress;
       port: uint16;}
      with
        override ToString : unit -> string
        static member
          Create : proto:Protocol * ip:string * port:int -> HttpBinding
      end
    type WebPart = HttpRequest -> Async<unit> option
    type ErrorHandler =
      System.Exception -> System.String -> HttpRequest -> Async<unit>
    type Config =
      {bindings: HttpBinding list;
       error_handler: ErrorHandler;
       timeout: System.TimeSpan;
       ct: System.Threading.CancellationToken;}
    exception InternalFailure of string
  end
