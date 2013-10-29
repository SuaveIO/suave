namespace Suave
module Http =

  // generic 'response' and 'response_f' f-ns

  /// Respond with a given status code, http message, content in the body to a http request.
  val response_f : status_code:int -> reason_phrase:string -> f_content:(Types.HttpRequest -> Async<unit>) -> request:Types.HttpRequest -> Async<unit>

  /// Respond with a given status code, http reason phrase, content in the body to a http request.
  val response : status_code:int -> reason_phrase:string -> content:byte [] -> request:Types.HttpRequest -> Async<unit>

// Suave response modifiers/applicatives follow

  /// Sets a header with the key and value specified
  val set_header : key:string -> value:string -> http_request:Types.HttpRequest -> Types.HttpRequest

  /// Sets a cookie with the passed value in the 'cookie' parameter
  val set_cookie : cookie:string -> (Types.HttpRequest -> Types.HttpRequest)

// Suave filters/applicatives follow

  /// Match on the url
  val url : s:string -> x:Types.HttpRequest -> Types.HttpRequest option

  /// Match on the method
  val meth0d : s:string -> x:Types.HttpRequest -> Types.HttpRequest option

  /// <summary>
  /// Match on GET requests.
  /// <para>The GET method means retrieve whatever information (in the form of an entity) is
  /// identified by the Request-URI. If the Request-URI refers to a data-producing process,
  /// it is the produced data which shall be returned as the entity in the response and
  /// not the source text of the process, unless that text happens to be the output of
  /// the process.
  /// </para><para>
  /// The semantics of the GET method change to a "conditional GET" if the request
  /// message includes an If-Modified-Since, If-Unmodified-Since, If-Match, If-None-Match,
  /// or If-Range header field. A conditional GET method requests that the entity be
  /// transferred only under the circumstances described by the conditional header field(s).
  /// The conditional GET method is intended to reduce unnecessary network usage by
  /// allowing cached entities to be refreshed without requiring multiple requests
  /// or transferring data already held by the client.
  /// </para></summary>
  val GET : x:Types.HttpRequest -> Types.HttpRequest option

  /// <summary>
  /// <para>Match on POST requests.</para>
  /// <para>
  /// The POST method is used to request that the origin server accept the entity enclosed
  /// in the request as a new subordinate of the resource identified by the Request-URI in
  /// the Request-Line. POST is designed to allow a uniform method to cover the following
  /// functions:
  /// </para>
  /// <list>
  /// <item>Annotation of existing resources;</item>
  /// <item>Posting a message to a bulletin board, newsgroup, mailing list, similar group
  /// of articles;</item>
  /// <item>Providing a block of data, such as the result of submitting a form, to a
  /// data-handling process;</item>
  /// <item>Extending a database through an append operation.</item>
  /// </list>
  /// <para>
  /// The actual function performed by the POST method is determined by the server and is
  /// usually dependent on the Request-URI. The posted entity is subordinate to that URI in
  /// the same way that a file is subordinate to a directory containing it, a news article
  /// is subordinate to a newsgroup to which it is posted, or a record is subordinate to a
  /// database.
  /// </para><para>
  /// The action performed by the POST method might not result in a resource that can be
  /// identified by a URI. In this case, either 200 (OK) or 204 (No Content) is the appropriate
  /// response status, depending on whether or not the response includes an entity that
  /// describes the result.
  /// </para><para>
  /// If a resource has been created on the origin server, the response SHOULD be 201 (Created)
  /// and contain an entity which describes the status of the request and refers to the
  /// new resource, and a Location header (see section 14.30).
  /// </para><para>
  /// Responses to this method are not cacheable, unless the response includes appropriate
  /// Cache-Control or Expires header fields. However, the 303 (See Other) response can be
  /// used to direct the user agent to retrieve a cacheable resource.
  /// </para>
  /// </summary>
  val POST : x:Types.HttpRequest -> Types.HttpRequest option

  /// <summary><para>
  /// Match on DELETE requests.
  /// </para><para>
  /// The DELETE method requests that the origin server delete the resource identified by
  /// the Request-URI. This method MAY be overridden by human intervention (or other means)
  /// on the origin server. The client cannot be guaranteed that the operation has been
  /// carried out, even if the status code returned from the origin server indicates that
  /// the action has been completed successfully. However, the server SHOULD NOT indicate
  /// success unless, at the time the response is given, it intends to delete the resource
  /// or move it to an inaccessible location.
  /// </para><para>
  /// A successful response SHOULD be 200 (OK) if the response includes an entity describing
  /// the status, 202 (Accepted) if the action has not yet been enacted, or 204 (No Content)
  /// if the action has been enacted but the response does not include an entity.
  /// </para><para>
  /// If the request passes through a cache and the Request-URI identifies one or more
  /// currently cached entities, those entries SHOULD be treated as stale. Responses to this
  /// method are not cacheable. 
  /// </para>
  val DELETE : x:Types.HttpRequest -> Types.HttpRequest option

  /// <summary><para>
  /// Match on PUT requests
  /// </para><para>
  /// The PUT method requests that the enclosed entity be stored under the supplied
  /// Request-URI. If the Request-URI refers to an already existing resource, the
  /// enclosed entity SHOULD be considered as a modified version of the one residing
  /// on the origin server. If the Request-URI does not point to an existing resource,
  /// and that URI is capable of being defined as a new resource by the requesting user
  /// agent, the origin server can create the resource with that URI. If a new resource
  /// is created, the origin server MUST inform the user agent via the 201 (Created)
  /// response. If an existing resource is modified, either the 200 (OK) or 204 (No Content)
  /// response codes SHOULD be sent to indicate successful completion of the request. If
  /// the resource could not be created or modified with the Request-URI, an appropriate
  /// error response SHOULD be given that reflects the nature of the problem. The recipient
  /// of the entity MUST NOT ignore any Content-* (e.g. Content-Range) headers that it does
  /// not understand or implement and MUST return a 501 (Not Implemented) response in such cases.
  /// </para><para>
  /// If the request passes through a cache and the Request-URI identifies one or more
  /// currently cached entities, those entries SHOULD be treated as stale. Responses to
  /// this method are not cacheable.
  /// </para><para>
  /// The fundamental difference between the POST and PUT requests is reflected in the
  /// different meaning of the Request-URI. The URI in a POST request identifies the
  /// resource that will handle the enclosed entity. That resource might be a data-accepting
  /// process, a gateway to some other protocol, or a separate entity that accepts annotations.
  /// In contrast, the URI in a PUT request identifies the entity enclosed with the
  /// request -- the user agent knows what URI is intended and the server MUST NOT attempt
  /// to apply the request to some other resource. If the server desires that the request
  /// be applied to a different URI, it MUST send a 301 (Moved Permanently) response;
  /// the user agent MAY then make its own decision regarding whether or not to redirect
  /// the request.
  /// </para><para>
  /// A single resource MAY be identified by many different URIs. For example, an article
  /// might have a URI for identifying "the current version" which is separate from the URI
  /// identifying each particular version. In this case, a PUT request on a general URI might
  /// result in several other URIs being defined by the origin server.
  /// </para><para>
  /// PUT requests MUST obey the message transmission requirements set out in section 8.2.
  /// </para><para>
  /// HTTP/1.1 does not define how a PUT method affects the state of an origin server.
  /// </para><para>
  /// Unless otherwise specified for a particular entity-header, the entity-headers in the
  /// PUT request SHOULD be applied to the resource created or modified by the PUT.
  /// </para></summary>
  val PUT : x:Types.HttpRequest -> Types.HttpRequest option

  /// <summary><para>
  /// Match on HEAD requests.
  /// </para><para>
  /// The HEAD method is identical to GET except that the server MUST NOT return a message-body
  /// in the response. The metainformation contained in the HTTP headers in response to a
  /// HEAD request SHOULD be identical to the information sent in response to a GET request.
  /// This method can be used for obtaining metainformation about the entity implied by the
  /// request without transferring the entity-body itself. This method is often used for
  /// testing hypertext links for validity, accessibility, and recent modification.
  /// </para><para>
  /// The response to a HEAD request MAY be cacheable in the sense that the information
  /// contained in the response MAY be used to update a previously cached entity from that
  /// resource. If the new field values indicate that the cached entity differs from the
  /// current entity (as would be indicated by a change in Content-Length, Content-MD5,
  /// ETag or Last-Modified), then the cache MUST treat the cache entry as stale.
  /// </para></summary>
  val HEAD : x:Types.HttpRequest -> Types.HttpRequest option

  /// <summary><para>
  /// Match on CONNECT requests.
  /// </para><para>
  /// This specification (RFC 2616) reserves the method name CONNECT for use with a
  /// proxy that can dynamically switch to being a tunnel (e.g. SSL tunneling [44]).
  /// </para></summary>
  val CONNECT : x:Types.HttpRequest -> Types.HttpRequest option

  /// <summary><para>
  /// Match on PATCH requests.
  /// </para><para>
  /// The PATCH method requests that a set of changes described in the
  /// request entity be applied to the resource identified by the Request-
  /// URI.  The set of changes is represented in a format called a "patch
  /// document" identified by a media type.  If the Request-URI does not
  /// point to an existing resource, the server MAY create a new resource,
  /// depending on the patch document type (whether it can logically modify
  /// a null resource) and permissions, etc.
  /// </para><para>
  /// The difference between the PUT and PATCH requests is reflected in the
  /// way the server processes the enclosed entity to modify the resource
  /// identified by the Request-URI.  In a PUT request, the enclosed entity
  /// is considered to be a modified version of the resource stored on the
  /// origin server, and the client is requesting that the stored version
  /// be replaced.  With PATCH, however, the enclosed entity contains a set
  /// of instructions describing how a resource currently residing on the
  /// origin server should be modified to produce a new version.  The PATCH
  /// method affects the resource identified by the Request-URI, and it
  /// also MAY have side effects on other resources; i.e., new resources
  /// may be created, or existing ones modified, by the application of a
  /// PATCH.
  /// </para></summary>
  /// <remarks>From http://tools.ietf.org/html/rfc5789#page-2</remarks>
  val PATCH : x:Types.HttpRequest -> Types.HttpRequest option

  /// <summary><para>
  /// Match on TRACE requests.
  /// </para><para>
  /// The TRACE method is used to invoke a remote, application-layer loop- back of the
  /// request message. The final recipient of the request SHOULD reflect the message
  /// received back to the client as the entity-body of a 200 (OK) response. The final
  /// recipient is either the origin server or the first proxy or gateway to receive a
  /// Max-Forwards value of zero (0) in the request (see section 14.31). A TRACE request
  /// MUST NOT include an entity.
  /// </para><para>
  /// TRACE allows the client to see what is being received at the other end of the request
  /// chain and use that data for testing or diagnostic information. The value of the Via
  /// header field (section 14.45) is of particular interest, since it acts as a trace of
  /// the request chain. Use of the Max-Forwards header field allows the client to limit
  /// the length of the request chain, which is useful for testing a chain of proxies forwarding
  /// messages in an infinite loop.
  /// </para><para>
  /// If the request is valid, the response SHOULD contain the entire request message in
  /// the entity-body, with a Content-Type of "message/http". Responses to this method
  /// MUST NOT be cached.
  /// </para></summary>
  val TRACE : x:Types.HttpRequest -> Types.HttpRequest option

  /// Match on OPTIONS requests
  /// The OPTIONS method represents a request for information about the communication
  /// options available on the request/response chain identified by the Request-URI.
  /// This method allows the client to determine the options and/or requirements associated
  /// with a resource, or the capabilities of a server, without implying a resource
  /// action or initiating a resource retrieval.
  /// Responses to this method are not cacheable.
  val OPTIONS : x:Types.HttpRequest -> Types.HttpRequest option

  /// <summary><para>
  /// 200
  /// </para><para>
  /// Write the bytes to the body as a byte array
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val ok : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// 200
  /// </para><para>
  /// Write the string to the body as UTF-8
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val OK : a:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// 201
  /// </para><para>
  /// Write the bytes to the body as a byte array
  /// </para></para>
  /// The request has been fulfilled and resulted in a new resource being
  /// created. The newly created resource can be referenced by the URI(s)
  /// returned in the entity of the response, with the most specific URI
  /// for the resource given by a Location header field. The response
  /// SHOULD include an entity containing a list of resource
  /// characteristics and location(s) from which the user or user agent can
  /// choose the one most appropriate. The entity format is specified by
  /// the media type given in the Content-Type header field. The origin
  /// server MUST create the resource before returning the 201 status code.
  /// If the action cannot be carried out immediately, the server SHOULD
  /// respond with 202 (Accepted) response instead.
  /// </para><para>
  /// A 201 response MAY contain an ETag response header field indicating
  /// the current value of the entity tag for the requested variant just
  /// created, see section 14.19.
  /// </para></summary>
  val created : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// 201
  /// </para><para>
  /// The request has been fulfilled and resulted in a new resource being
  /// created. The newly created resource can be referenced by the URI(s)
  /// returned in the entity of the response, with the most specific URI
  /// for the resource given by a Location header field. The response
  /// SHOULD include an entity containing a list of resource
  /// characteristics and location(s) from which the user or user agent can
  /// choose the one most appropriate. The entity format is specified by
  /// the media type given in the Content-Type header field. The origin
  /// server MUST create the resource before returning the 201 status code.
  /// If the action cannot be carried out immediately, the server SHOULD
  /// respond with 202 (Accepted) response instead.
  /// </para><para>
  /// A 201 response MAY contain an ETag response header field indicating
  /// the current value of the entity tag for the requested variant just
  /// created, see section 14.19.
  /// </para></summary>
  val CREATED : s:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// 202
  /// </para><para>
  /// The request has been accepted for processing, but the processing has
  /// not been completed. The request might or might not eventually be
  /// acted upon, as it might be disallowed when processing actually takes
  /// place. There is no facility for re-sending a status code from an
  /// asynchronous operation such as this.
  /// </para><para>
  /// The 202 response is intentionally non-committal. Its purpose is to
  /// allow a server to accept a request for some other process (perhaps a
  /// batch-oriented process that is only run once per day) without
  /// requiring that the user agent's connection to the server persist
  /// until the process is completed. The entity returned with this
  /// response SHOULD include an indication of the request's current status
  /// and either a pointer to a status monitor or some estimate of when the
  /// user can expect the request to be fulfilled.
  /// </para></summary>
  val accepted : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// 202
  /// </para><para>
  /// The request has been accepted for processing, but the processing has
  /// not been completed. The request might or might not eventually be
  /// acted upon, as it might be disallowed when processing actually takes
  /// place. There is no facility for re-sending a status code from an
  /// asynchronous operation such as this.
  /// </para><para>
  /// The 202 response is intentionally non-committal. Its purpose is to
  /// allow a server to accept a request for some other process (perhaps a
  /// batch-oriented process that is only run once per day) without
  /// requiring that the user agent's connection to the server persist
  /// until the process is completed. The entity returned with this
  /// response SHOULD include an indication of the request's current status
  /// and either a pointer to a status monitor or some estimate of when the
  /// user can expect the request to be fulfilled.
  /// </para></summary>
  val ACCEPTED : s:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// 204
  /// </para><para>
  /// The server has fulfilled the request but does not need to return an
  /// entity-body, and might want to return updated metainformation. The
  /// response MAY include new or updated metainformation in the form of
  /// entity-headers, which if present SHOULD be associated with the
  /// requested variant.
  /// </para><para>
  /// If the client is a user agent, it SHOULD NOT change its document view
  /// from that which caused the request to be sent. This response is
  /// primarily intended to allow input for actions to take place without
  /// causing a change to the user agent's active document view, although
  /// any new or updated metainformation SHOULD be applied to the document
  /// currently in the user agent's active view.
  /// </para><para>
  /// The 204 response MUST NOT include a message-body, and thus is always
  /// terminated by the first empty line after the header fields.
  /// </para></summary>
  val no_content : (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// 204
  /// </para><para>
  /// The server has fulfilled the request but does not need to return an
  /// entity-body, and might want to return updated metainformation. The
  /// response MAY include new or updated metainformation in the form of
  /// entity-headers, which if present SHOULD be associated with the
  /// requested variant.
  /// </para><para>
  /// If the client is a user agent, it SHOULD NOT change its document view
  /// from that which caused the request to be sent. This response is
  /// primarily intended to allow input for actions to take place without
  /// causing a change to the user agent's active document view, although
  /// any new or updated metainformation SHOULD be applied to the document
  /// currently in the user agent's active view.
  /// </para><para>
  /// The 204 response MUST NOT include a message-body, and thus is always
  /// terminated by the first empty line after the header fields.
  /// </para></summary>
  val NO_CONTENT : (Types.HttpRequest -> Async<unit> option)

(*
10.3 Redirection 3xx

   This class of status code indicates that further action needs to be
   taken by the user agent in order to fulfill the request.  The action
   required MAY be carried out by the user agent without interaction
   with the user if and only if the method used in the second request is
   GET or HEAD. A client SHOULD detect infinite redirection loops, since
   such loops generate network traffic for each redirection.

      Note: previous versions of this specification recommended a
      maximum of five redirections. Content developers should be aware
      that there might be clients that implement such a fixed
      limitation.
*)

  /// <summary><para>
  /// 301
  /// </para><para>
  /// The requested resource has been assigned a new permanent URI and any
  /// future references to this resource SHOULD use one of the returned
  /// URIs.  Clients with link editing capabilities ought to automatically
  /// re-link references to the Request-URI to one or more of the new
  /// references returned by the server, where possible. This response is
  /// cacheable unless indicated otherwise.
  /// </para><para>
  /// The new permanent URI SHOULD be given by the Location field in the
  /// response. Unless the request method was HEAD, the entity of the
  /// response SHOULD contain a short hypertext note with a hyperlink to
  /// the new URI(s).
  /// </para><para>
  /// If the 301 status code is received in response to a request other
  /// than GET or HEAD, the user agent MUST NOT automatically redirect the
  /// request unless it can be confirmed by the user, since this might
  /// change the conditions under which the request was issued.
  /// </para></summary>
  /// <remarks>
  ///    Note: When automatically redirecting a POST request after
  ///    receiving a 301 status code, some existing HTTP/1.0 user agents
  ///    will erroneously change it into a GET request.
  /// </remarks>
  val moved_permanently : location:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// 301
  /// </para><para>
  /// The requested resource has been assigned a new permanent URI and any
  /// future references to this resource SHOULD use one of the returned
  /// URIs.  Clients with link editing capabilities ought to automatically
  /// re-link references to the Request-URI to one or more of the new
  /// references returned by the server, where possible. This response is
  /// cacheable unless indicated otherwise.
  /// </para><para>
  /// The new permanent URI SHOULD be given by the Location field in the
  /// response. Unless the request method was HEAD, the entity of the
  /// response SHOULD contain a short hypertext note with a hyperlink to
  /// the new URI(s).
  /// </para><para>
  /// If the 301 status code is received in response to a request other
  /// than GET or HEAD, the user agent MUST NOT automatically redirect the
  /// request unless it can be confirmed by the user, since this might
  /// change the conditions under which the request was issued.
  /// </para></summary>
  /// <remarks>
  ///    Note: When automatically redirecting a POST request after
  ///    receiving a 301 status code, some existing HTTP/1.0 user agents
  ///    will erroneously change it into a GET request.
  /// </remarks>
  val MOVED_PERMANENTLY : location:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// The requested resource resides temporarily under a different URI.
  /// Since the redirection might be altered on occasion, the client SHOULD
  /// continue to use the Request-URI for future requests.  This response
  /// is only cacheable if indicated by a Cache-Control or Expires header
  /// field.
  /// </para><para>
  /// The temporary URI SHOULD be given by the Location field in the
  /// response. Unless the request method was HEAD, the entity of the
  /// response SHOULD contain a short hypertext note with a hyperlink to
  /// the new URI(s).
  /// </para><para>
  /// If the 302 status code is received in response to a request other
  /// than GET or HEAD, the user agent MUST NOT automatically redirect the
  /// request unless it can be confirmed by the user, since this might
  /// change the conditions under which the request was issued.
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val found : location:string -> (Types.HttpRequest -> Async<unit>)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val FOUND : location:string -> (Types.HttpRequest -> Async<unit>)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val not_modified : unit -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val NOT_MODIFIED : unit -> (unit -> Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val bad_request : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val BAD_REQUEST : s:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val unauthorized : s:byte [] -> (Types.HttpRequest -> Async<unit>)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val UNAUTHORIZED : s:string -> (Types.HttpRequest -> Async<unit>)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val challenge : (Types.HttpRequest -> Async<unit>)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val forbidden : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val FORBIDDEN : s:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val not_found : message:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val NOT_FOUND : message:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val method_not_allowed : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val METHOD_NOT_ALLOWED : s:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val gone : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val GONE : s:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val unsupported_media_type : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val UNSUPPORTED_MEDIA_TYPE : s:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val unprocessable_entity : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val UNPROCESSABLE_ENTITY : s:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val too_many_requests : s:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val TOO_MANY_REQUESTS : s:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val internal_error : message:byte [] -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val INTERNAL_ERROR : a:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val redirect : url:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val mime_type : _arg1:string -> string

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val set_mime_type : t:string -> (Types.HttpRequest -> Types.HttpRequest)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val send_file : filename:string -> r:Types.HttpRequest -> Async<unit> option

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val file : filename:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val local_file : fileName:string -> string

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val browse_file : filename:string -> (Types.HttpRequest -> Async<unit> option)

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val browse : Types.WebPart

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  type WebResult = Option<Async<unit>>


  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val dir : req:Types.HttpRequest -> WebResult

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val close_pipe : p:Types.HttpRequest option -> unit

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val parse_authentication_token : token:string -> string * string * string

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val authenticate_basic : f:(Types.HttpRequest -> bool) -> p:Types.HttpRequest -> Async<unit> option

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val log : s:System.IO.Stream -> http_request:Types.HttpRequest -> Types.HttpRequest option

  /// <summary><para>
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val urlscan : pf:PrintfFormat<'a,'b,'c,'d,'t> -> h:('t -> Types.WebPart) -> Types.WebPart
