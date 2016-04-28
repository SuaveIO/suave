namespace Suave

open Suave.Sockets

/// <summary><para>
/// The HTTP module has these main sub-modules:
/// </para>
/// <list>
///   <item>Response - response and response_f functions.</item>
///   <item>Writers - ways to modify the response.</item>
///   <item>Intermediate - 100 and 101 response codes.</item>
///   <item>Successful- 2xx response codes.</item>
///   <item>Redirection - 3xx response codes.</item>
///   <item>RequestErrors - 4xx response codes.</item>
///   <item>ServerErrors - 5xx response codes.</item>
///   <item>Applicatives - use to filter down the request to something you want</item>
///   <item>Files - send files to the client</item>
///   <item>Authentication - Methods for authenticating http requests</item>
/// </list>
/// <para>and these main types:</para>
/// <list>
///   <item>WebResult = Async&lt;unit&gt; option</item>
///   <item>WebPart = HttpContext -&gt; WebResult</item>
///   <item>HttpMethod</item>
/// </list>
/// </summary>

/// general response functions
module Response =

  /// Respond with a given status code, http message, content in the body to a http request.
  // val response_f : status_code:HttpCode -> ( Connection -> SocketOp<unit>) -> request:HttpContext -> (Connection -> SocketOp<unit>)

  /// Respond with a given status code, http reason phrase, content in the body to a http request.
  val response : statusCode:HttpCode -> content:byte [] -> WebPart

/// Module that allows changing the output response in different ways.
/// Functions have signature f :: params... -> HttpContext -> HttpContext.
module Writers =

  /// Sets the HTTP response status
  val setStatus : status:HttpCode -> WebPart

  /// Ensures that the header named by the `key` parameter has the given value,
  /// by removing any other headers with the same key from the response header
  /// list.
  val setHeader : key:string -> value:string -> WebPart

  /// Ensures that the is unique in the comma-separated list of the response
  /// header, denoted by `key`. This is useful for example for the "Vary" header
  /// which should at the very minimum include "Accept-Encoding", but may also
  /// include "Accept-Language" for multi-lingual sites (see Suave.Locale repo)
  /// and "Authorization" and "Cookie" for authorized resources.
  val setHeaderValue : key:string -> value:string -> WebPart

  /// Adds the header key with the given value to the list of returned headers,
  /// even if that header already exists. This means that Suave will serve a
  /// a response with the header denoted by `key` with possibly different values.
  ///
  /// Also consider `setHeader` and `setHeaderValue` depending on what semantics
  /// you'd like.
  ///
  /// Furthermore, Cookies must be set on separate header lines (using this
  /// function) and not comma-concatenated. See
  /// https://github.com/SuaveIO/suave/issues/338#issuecomment-156820747 for
  /// defails.
  val addHeader : key:string -> value:string -> WebPart

  /// Sets a user data key-value pair with the key and value specified. Downstream
  /// web parts can read this.
  val setUserData : key:string -> value:'T -> WebPart

  /// Unset the user data by the given key
  val unsetUserData : key : string -> WebPart

  /// <summary>
  /// Creates a MIME type record
  /// </summary>
  val mkMimeType : name:string -> compression:bool -> MimeType option

  /// <summary><para>
  /// Map a file ending to a mime-type
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val defaultMimeTypesMap : ext:string -> MimeType option

  /// <summary><para>
  /// Set the Content-Type header to the mime type given. Remember that it should
  /// include the encoding of your content. So for example, specifying a mimeType
  /// value of 'application/json; charset=utf-8' is strongly recommended (but
  /// replace 'json' with your own MIME type, of course ;))
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val setMimeType : mimeType:string -> WebPart

// http://www.web-cache.com/Writings/http-status-codes.html

/// Control-flow functions, such as 100 Continue and 101 Switching Protocol.
module Intermediate =

  /// <summary><para>
  /// 100
  /// </para><para>
  /// The purpose of the 100 (Continue) status is to allow a client that is sending a request message with a
  /// request body to determine if the origin server is willing to accept the request (based on the request headers)
  /// before the client sends the request body
  /// </para></summary>
  val CONTINUE : WebPart

  /// <summary><para>
  /// 101
  /// </para><para>
  /// The server will switch protocols to those defined by the response's Upgrade header field immediately after the
  /// empty line which terminates the 101 response.
  /// </para></summary>
  val SWITCHING_PROTO : WebPart

/// <summary><para>
/// 2xx successful responses
/// </para><para>
/// Functions have signature <code>f :: params... -&gt; HttpContext -&gt; Async&lt;unit&gt; option</code>.
/// </para><para>
/// Functions from here are 'end routes' in that they don't require you to keep
/// returning applicatives, but can end up in an async monad/workflow that writes
/// the data to the client in the end.
/// </para></summary>
module Successful =
  /// <summary><para>
  /// 200
  /// </para><para>
  /// Write the bytes to the body as a byte array
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val ok : bytes:byte [] -> WebPart

  /// <summary><para>
  /// 200
  /// </para><para>
  /// Write the string to the body as UTF-8
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val OK : body:string -> WebPart

  /// <summary><para>
  /// 201
  /// </para><para>
  /// Write the bytes to the body as a byte array
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
  val created : bytes:byte [] -> WebPart

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
  val CREATED : body:string -> WebPart

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
  val accepted : bytes:byte [] -> WebPart

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
  val ACCEPTED : body:string -> WebPart

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
  val no_content : WebPart

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
  val NO_CONTENT : WebPart

/// <summary><para>
/// Functions have signature <code>f :: params... -&gt; HttpContext -&gt; Async&lt;unit&gt; option</code>.
/// <para></para>
///
/// Functions from here are 'end routes' in that they don't require you to keep
/// returning applicatives, but can end up in an async monad/workflow that writes
/// the data to the client in the end.
/// <para></para>
///
/// This class of status code indicates that further action needs to be
/// taken by the user agent in order to fulfill the request.  The action
/// required MAY be carried out by the user agent without interaction
/// with the user if and only if the method used in the second request is
/// GET or HEAD. A client SHOULD detect infinite redirection loops, since
/// such loops generate network traffic for each redirection.</para></summary>
/// <remarks>
///    Note: previous versions of this specification recommended a
///    maximum of five redirections. Content developers should be aware
///    that there might be clients that implement such a fixed
///    limitation.
/// </remarks>
module Redirection =

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
  val moved_permanently : location:string -> WebPart

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
  val MOVED_PERMANENTLY : location:string -> WebPart

  /// <summary><para>
  /// 302
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
  /// </para><para>
  /// If the 302 status code is received in response to a request other
  /// than GET or HEAD, the user agent MUST NOT automatically redirect the
  /// request unless it can be confirmed by the user, since this might
  /// change the conditions under which the request was issued.
  /// </para></summary>
  /// <remarks>
  ///    Note: RFC 1945 and RFC 2068 specify that the client is not allowed
  ///    to change the method on the redirected request.  However, most
  ///    existing user agent implementations treat 302 as if it were a 303
  ///    response, performing a GET on the Location field-value regardless
  ///    of the original request method. The status codes 303 and 307 have
  ///    been added for servers that wish to make unambiguously clear which
  ///    kind of reaction is expected of the client.
  /// </remarks>
  val found : location:string -> WebPart

  /// <summary><para>
  /// 302
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
  /// </para><para>
  /// If the 302 status code is received in response to a request other
  /// than GET or HEAD, the user agent MUST NOT automatically redirect the
  /// request unless it can be confirmed by the user, since this might
  /// change the conditions under which the request was issued.
  /// </para></summary>
  /// <remarks>
  ///    Note: RFC 1945 and RFC 2068 specify that the client is not allowed
  ///    to change the method on the redirected request.  However, most
  ///    existing user agent implementations treat 302 as if it were a 303
  ///    response, performing a GET on the Location field-value regardless
  ///    of the original request method. The status codes 303 and 307 have
  ///    been added for servers that wish to make unambiguously clear which
  ///    kind of reaction is expected of the client.
  /// </remarks>
  val FOUND : location:string -> WebPart
  
  /// <summary><para>
  /// Composite:
  /// </para><para>
  /// HTTP/1.1 302 Found
  /// </para><para>
  /// Location: 'location'
  /// </para><para>
  /// Content-Type: text/html; charset=utf-8
  /// </para><para>
  /// &lt;html&gt;
  ///   &lt;body&gt;
  ///    &lt;a href=&quot;'location'&quot;&gt;Content Moved&lt;/a&gt;
  ///   &lt;/body&gt;
  /// &lt;/html&gt;
  /// </para><para>
  /// Redirect the request to another location specified by the url parameter.
  /// Sets the Location header and returns 302 Content Moved status-code/reason phrase.
  /// </para></summary>
  val redirect : location:string -> WebPart

  /// <summary><para>
  /// If the client has performed a conditional GET request and access is
  /// allowed, but the document has not been modified, the server SHOULD
  /// respond with this status code. The 304 response MUST NOT contain a
  /// message-body, and thus is always terminated by the first empty line
  /// after the header fields.
  /// </para><para>
  /// The response MUST include the following header fields:
  /// <list><item>
  ///   Date, unless its omission is required by section 14.18.1.
  ///   If a clockless origin server obeys these rules, and proxies and
  ///   clients add their own Date to any response received without one (as
  ///   already specified by [RFC 2068], section 14.19), caches will operate
  ///   correctly.
  /// </item><item>
  ///   ETag and/or Content-Location, if the header would have been sent
  ///   in a 200 response to the same request
  /// </item><item>
  ///   Expires, Cache-Control, and/or Vary, if the field-value might
  ///   differ from that sent in any previous response for the same
  ///   variant
  /// </item></list>
  /// </para><para>
  /// If the conditional GET used a strong cache validator (see section
  /// 13.3.3), the response SHOULD NOT include other entity-headers.
  /// Otherwise (i.e., the conditional GET used a weak validator), the
  /// response MUST NOT include other entity-headers; this prevents
  /// inconsistencies between cached entity-bodies and updated headers.
  /// </para><para>
  /// If a 304 response indicates an entity not currently cached, then the
  /// cache MUST disregard the response and repeat the request without the
  /// conditional.
  /// </para><para>
  /// If a cache uses a received 304 response to update a cache entry, the
  /// cache MUST update the entry to reflect any new field values given in
  /// the response.
  /// </para></summary>
  val not_modified : WebPart

  /// <summary><para>
  /// If the client has performed a conditional GET request and access is
  /// allowed, but the document has not been modified, the server SHOULD
  /// respond with this status code. The 304 response MUST NOT contain a
  /// message-body, and thus is always terminated by the first empty line
  /// after the header fields.
  /// </para><para>
  /// The response MUST include the following header fields:
  /// <list><item>
  ///   Date, unless its omission is required by section 14.18.1.
  ///   If a clockless origin server obeys these rules, and proxies and
  ///   clients add their own Date to any response received without one (as
  ///   already specified by [RFC 2068], section 14.19), caches will operate
  ///   correctly.
  /// </item><item>
  ///   ETag and/or Content-Location, if the header would have been sent
  ///   in a 200 response to the same request
  /// </item><item>
  ///   Expires, Cache-Control, and/or Vary, if the field-value might
  ///   differ from that sent in any previous response for the same
  ///   variant
  /// </item></list>
  /// </para><para>
  /// If the conditional GET used a strong cache validator (see section
  /// 13.3.3), the response SHOULD NOT include other entity-headers.
  /// Otherwise (i.e., the conditional GET used a weak validator), the
  /// response MUST NOT include other entity-headers; this prevents
  /// inconsistencies between cached entity-bodies and updated headers.
  /// </para><para>
  /// If a 304 response indicates an entity not currently cached, then the
  /// cache MUST disregard the response and repeat the request without the
  /// conditional.
  /// </para><para>
  /// If a cache uses a received 304 response to update a cache entry, the
  /// cache MUST update the entry to reflect any new field values given in
  /// the response.
  /// </para></summary>
  val NOT_MODIFIED : WebPart

/// <summary><para>10.4 Client Error 4xx</para>
///
/// <para>The 4xx class of status code is intended for cases in which the
/// client seems to have erred. Except when responding to a HEAD request,
/// the server SHOULD include an entity containing an explanation of the
/// error situation, and whether it is a temporary or permanent
/// condition. These status codes are applicable to any request method.
/// User agents SHOULD display any included entity to the user.
/// <para></para>
/// If the client is sending data, a server implementation using TCP
/// SHOULD be careful to ensure that the client acknowledges receipt of
/// the packet(s) containing the response, before the server closes the
/// input connection. If the client continues sending data to the server
/// after the close, the server's TCP stack will send a reset packet to
/// the client, which may erase the client's unacknowledged input buffers
/// before they can be read and interpreted by the HTTP application.
/// </para></summary>
module RequestErrors =

  /// <summary><para>
  /// 400
  /// </para><para>
  /// The request could not be understood by the server due to malformed
  /// syntax. The client SHOULD NOT repeat the request without
  /// modifications.
  /// </para></summary>
  val bad_request : bytes:byte [] -> WebPart

  /// <summary><para>
  /// 400
  /// </para><para>
  /// The request could not be understood by the server due to malformed
  /// syntax. The client SHOULD NOT repeat the request without
  /// modifications.
  /// </para></summary>
  val BAD_REQUEST : body:string -> WebPart

  /// <summary><para>
  /// 401
  /// </para><para>
  /// The request requires user authentication. The response MUST include a
  /// WWW-Authenticate header field (section 14.47) containing a challenge
  /// applicable to the requested resource. The client MAY repeat the
  /// request with a suitable Authorization header field (section 14.8). If
  /// the request already included Authorization credentials, then the 401
  /// response indicates that authorization has been refused for those
  /// credentials. If the 401 response contains the same challenge as the
  /// prior response, and the user agent has already attempted
  /// authentication at least once, then the user SHOULD be presented the
  /// entity that was given in the response, since that entity might
  /// include relevant diagnostic information. HTTP access authentication
  /// is explained in "HTTP Authentication: Basic and Digest Access
  /// Authentication" [43].
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val unauthorized : bytes:byte [] -> WebPart

  /// <summary><para>
  /// 401
  /// </para><para>
  /// The request requires user authentication. The response MUST include a
  /// WWW-Authenticate header field (section 14.47) containing a challenge
  /// applicable to the requested resource. The client MAY repeat the
  /// request with a suitable Authorization header field (section 14.8). If
  /// the request already included Authorization credentials, then the 401
  /// response indicates that authorization has been refused for those
  /// credentials. If the 401 response contains the same challenge as the
  /// prior response, and the user agent has already attempted
  /// authentication at least once, then the user SHOULD be presented the
  /// entity that was given in the response, since that entity might
  /// include relevant diagnostic information. HTTP access authentication
  /// is explained in "HTTP Authentication: Basic and Digest Access
  /// Authentication" [43].
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val UNAUTHORIZED : body:string -> WebPart

  /// <summary><para>
  /// Composite:
  /// </para><para>
  /// HTTP/1.1 401 Unauthorized
  /// </para><para>
  /// WWW-Authenticate: Basic realm="protected"
  /// </para><para>
  /// A challenge response with a WWW-Authenticate header,
  /// and 401 Authorization Required response message.
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// Also see authenticate_basic and unauthorized
  /// </remarks>
  val challenge : WebPart

  /// <summary><para>
  /// 403
  /// </para><para>
  /// The server understood the request, but is refusing to fulfill it.
  /// Authorization will not help and the request SHOULD NOT be repeated.
  /// If the request method was not HEAD and the server wishes to make
  /// public why the request has not been fulfilled, it SHOULD describe the
  /// reason for the refusal in the entity. If the server does not wish to
  /// make this information available to the client, the status code 404
  /// (Not Found) can be used instead.
  /// </para></summary>
  val forbidden : bytes:byte [] -> WebPart

  /// <summary><para>
  /// 403
  /// </para><para>
  /// The server understood the request, but is refusing to fulfill it.
  /// Authorization will not help and the request SHOULD NOT be repeated.
  /// If the request method was not HEAD and the server wishes to make
  /// public why the request has not been fulfilled, it SHOULD describe the
  /// reason for the refusal in the entity. If the server does not wish to
  /// make this information available to the client, the status code 404
  /// (Not Found) can be used instead.
  /// </para></summary>
  val FORBIDDEN : body:string -> WebPart

  /// <summary><para>
  /// 404
  /// </para><para>
  /// Send a 404 Not Found with a byte array body specified by the 's' parameter.
  /// </para><para>
  /// The server has not found anything matching the Request-URI. No
  /// indication is given of whether the condition is temporary or
  /// permanent. The 410 (Gone) status code SHOULD be used if the server
  /// knows, through some internally configurable mechanism, that an old
  /// resource is permanently unavailable and has no forwarding address.
  /// This status code is commonly used when the server does not wish to
  /// reveal exactly why the request has been refused, or when no other
  /// response is applicable.
  /// </para></summary>
  val not_found : bytes:byte [] -> WebPart
  
  /// <summary><para>
  /// 404
  /// </para><para>
  /// Write the 'message' string to the body as UTF-8 encoded text, while
  /// returning 404 Not Found to the response
  /// </para><para>
  /// The server has not found anything matching the Request-URI. No
  /// indication is given of whether the condition is temporary or
  /// permanent. The 410 (Gone) status code SHOULD be used if the server
  /// knows, through some internally configurable mechanism, that an old
  /// resource is permanently unavailable and has no forwarding address.
  /// This status code is commonly used when the server does not wish to
  /// reveal exactly why the request has been refused, or when no other
  /// response is applicable.
  /// </para></summary>
  val NOT_FOUND : body:string -> WebPart

  /// <summary><para>
  /// 405
  /// </para><para>
  /// The method specified in the Request-Line is not allowed for the
  /// resource identified by the Request-URI. The response MUST include an
  /// Allow header containing a list of valid methods for the requested
  /// resource.
  /// </para></summary>
  val method_not_allowed : bytes:byte [] -> WebPart

  /// <summary><para>
  /// 405
  /// </para><para>
  /// The method specified in the Request-Line is not allowed for the
  /// resource identified by the Request-URI. The response MUST include an
  /// Allow header containing a list of valid methods for the requested
  /// resource.
  /// </para></summary>
  val METHOD_NOT_ALLOWED : body:string -> WebPart

  /// <summary><para>
  /// 406
  /// </para><para>
  /// The resource identified by the request is only capable of generating
  /// response entities which have content characteristics not acceptable
  /// according to the accept headers sent in the request.
  /// </para><para>
  /// Unless it was a HEAD request, the response SHOULD include an entity
  /// containing a list of available entity characteristics and location(s)
  /// from which the user or user agent can choose the one most
  /// appropriate. The entity format is specified by the media type given
  /// in the Content-Type header field. Depending upon the format and the
  /// capabilities of the user agent, selection of the most appropriate
  /// choice MAY be performed automatically. However, this specification
  /// does not define any standard for such automatic selection.
  /// </para><para>
  ///    Note: HTTP/1.1 servers are allowed to return responses which are
  ///    not acceptable according to the accept headers sent in the
  ///    request. In some cases, this may even be preferable to sending a
  ///    406 response. User agents are encouraged to inspect the headers of
  ///    an incoming response to determine if it is acceptable.
  /// </para><para>
  /// If the response could be unacceptable, a user agent SHOULD
  /// temporarily stop receipt of more data and query the user for a
  /// decision on further actions.
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val not_acceptable : bytes:byte[] -> WebPart
  
  /// <summary><para>
  /// 406
  /// </para><para>
  /// The resource identified by the request is only capable of generating
  /// response entities which have content characteristics not acceptable
  /// according to the accept headers sent in the request.
  /// </para><para>
  /// Unless it was a HEAD request, the response SHOULD include an entity
  /// containing a list of available entity characteristics and location(s)
  /// from which the user or user agent can choose the one most
  /// appropriate. The entity format is specified by the media type given
  /// in the Content-Type header field. Depending upon the format and the
  /// capabilities of the user agent, selection of the most appropriate
  /// choice MAY be performed automatically. However, this specification
  /// does not define any standard for such automatic selection.
  /// </para><para>
  ///    Note: HTTP/1.1 servers are allowed to return responses which are
  ///    not acceptable according to the accept headers sent in the
  ///    request. In some cases, this may even be preferable to sending a
  ///    406 response. User agents are encouraged to inspect the headers of
  ///    an incoming response to determine if it is acceptable.
  /// </para><para>
  /// If the response could be unacceptable, a user agent SHOULD
  /// temporarily stop receipt of more data and query the user for a
  /// decision on further actions.
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val NOT_ACCEPTABLE : body:string -> WebPart

  /// <summary><para>
  /// 408
  /// </para><para>
  /// The client did not produce a request within the time that the server
  /// was prepared to wait. The client MAY repeat the request without
  /// modifications at any later time.
  /// </para></summary>
  val request_timeout : WebPart

  /// <summary><para>
  /// 409
  /// </para><para>
  /// The request could not be completed due to a conflict with the current
  /// state of the resource. This code is only allowed in situations where
  /// it is expected that the user might be able to resolve the conflict
  /// and resubmit the request. The response body SHOULD include enough
  /// information for the user to recognize the source of the conflict.
  /// Ideally, the response entity would include enough information for the
  /// user or user agent to fix the problem; however, that might not be
  /// possible and is not required.
  /// </para><para>
  /// Conflicts are most likely to occur in response to a PUT request. For
  /// example, if versioning were being used and the entity being PUT
  /// included changes to a resource which conflict with those made by an
  /// earlier (third-party) request, the server might use the 409 response
  /// to indicate that it can't complete the request. In this case, the
  /// response entity would likely contain a list of the differences
  /// between the two versions in a format defined by the response
  /// Content-Type.
  /// </para></summary>
  val conflict : bytes:byte[] -> WebPart
  
  /// <summary><para>
  /// 409
  /// </para><para>
  /// The request could not be completed due to a conflict with the current
  /// state of the resource. This code is only allowed in situations where
  /// it is expected that the user might be able to resolve the conflict
  /// and resubmit the request. The response body SHOULD include enough
  /// information for the user to recognize the source of the conflict.
  /// Ideally, the response entity would include enough information for the
  /// user or user agent to fix the problem; however, that might not be
  /// possible and is not required.
  /// </para><para>
  /// Conflicts are most likely to occur in response to a PUT request. For
  /// example, if versioning were being used and the entity being PUT
  /// included changes to a resource which conflict with those made by an
  /// earlier (third-party) request, the server might use the 409 response
  /// to indicate that it can't complete the request. In this case, the
  /// response entity would likely contain a list of the differences
  /// between the two versions in a format defined by the response
  /// Content-Type.
  /// </para></summary>
  val CONFLICT : body:string -> WebPart

  /// <summary><para>
  /// 410
  /// </para><para>
  /// The requested resource is no longer available at the server and no
  /// forwarding address is known. This condition is expected to be
  /// considered permanent. Clients with link editing capabilities SHOULD
  /// delete references to the Request-URI after user approval. If the
  /// server does not know, or has no facility to determine, whether or not
  /// the condition is permanent, the status code 404 (Not Found) SHOULD be
  /// used instead. This response is cacheable unless indicated otherwise.
  /// </para><para>
  /// The 410 response is primarily intended to assist the task of web
  /// maintenance by notifying the recipient that the resource is
  /// intentionally unavailable and that the server owners desire that
  /// remote links to that resource be removed. Such an event is common for
  /// limited-time, promotional services and for resources belonging to
  /// individuals no longer working at the server's site. It is not
  /// necessary to mark all permanently unavailable resources as "gone" or
  /// to keep the mark for any length of time -- that is left to the
  /// discretion of the server owner.
  /// </para></summary>
  val gone : bytes:byte [] -> WebPart
  
  /// <summary><para>
  /// 410
  /// </para><para>
  /// The requested resource is no longer available at the server and no
  /// forwarding address is known. This condition is expected to be
  /// considered permanent. Clients with link editing capabilities SHOULD
  /// delete references to the Request-URI after user approval. If the
  /// server does not know, or has no facility to determine, whether or not
  /// the condition is permanent, the status code 404 (Not Found) SHOULD be
  /// used instead. This response is cacheable unless indicated otherwise.
  /// </para><para>
  /// The 410 response is primarily intended to assist the task of web
  /// maintenance by notifying the recipient that the resource is
  /// intentionally unavailable and that the server owners desire that
  /// remote links to that resource be removed. Such an event is common for
  /// limited-time, promotional services and for resources belonging to
  /// individuals no longer working at the server's site. It is not
  /// necessary to mark all permanently unavailable resources as "gone" or
  /// to keep the mark for any length of time -- that is left to the
  /// discretion of the server owner.
  /// </para></summary>
  val GONE : body:string -> WebPart

  /// <summary><para>
  /// 415
  /// </para><para>
  /// The server is refusing to service the request because the entity of
  /// the request is in a format not supported by the requested resource
  /// for the requested method.
  /// </para></summary>
  val unsupported_media_type : bytes:byte [] -> WebPart

  /// <summary><para>
  /// 415
  /// </para><para>
  /// The server is refusing to service the request because the entity of
  /// the request is in a format not supported by the requested resource
  /// for the requested method.
  /// </para></summary>
  val UNSUPPORTED_MEDIA_TYPE : body:string -> WebPart

  /// <summary><para>
  /// 422
  /// </para><para>
  /// The request was well-formed but was unable to be followed due to semantic errors.[4]
  /// </para><para>
  /// </para></summary>
  /// <remarks>(WebDAV; RFC 4918)</remarks>
  val unprocessable_entity : bytes:byte [] -> WebPart

  /// <summary><para>
  /// 422
  /// </para><para>
  /// The request was well-formed but was unable to be followed due to semantic errors.[4]
  /// </para><para>
  /// </para></summary>
  /// <remarks>(WebDAV; RFC 4918)</remarks>
  val UNPROCESSABLE_ENTITY : body:string -> WebPart

  /// <summary><para>
  /// 428
  /// </para><para>
  /// The 428 status code indicates that the origin server requires the
  /// request to be conditional.
  /// </para><para>
  /// Its typical use is to avoid the "lost update" problem, where a client
  /// GETs a resource's state, modifies it, and PUTs it back to the server,
  /// when meanwhile a third party has modified the state on the server,
  /// leading to a conflict.  By requiring requests to be conditional, the
  /// server can assure that clients are working with the correct copies.
  /// </para><para>
  /// Responses using this status code SHOULD explain how to resubmit the
  /// request successfully.  For example:
  /// <code>
  /// HTTP/1.1 428 Precondition Required
  /// Content-Type: text/html
  ///
  /// &lt;html&gt;
  ///    &lt;head&gt;
  ///       &lt;title&gt;Precondition Required&lt;/title&gt;
  ///    &lt;/head&gt;
  ///    &lt;body&gt;
  ///       &lt;h1&gt;Precondition Required&lt;/h1&gt;
  ///       &lt;p&gt;This request is required to be conditional;
  ///       try using &quot;If-Match&quot;.&lt;/p&gt;
  ///    &lt;/body&gt;
  /// &lt;/html&gt;
  /// </code>
  /// </para><para>
  /// Responses with the 428 status code MUST NOT be stored by a cache.
  /// </para></summary>
  /// <remarks>
  /// https://tools.ietf.org/html/rfc6585
  /// </remarks>
  val precondition_required : bytes:byte[] -> WebPart

  /// <summary><para>
  /// 428
  /// </para><para>
  /// The 428 status code indicates that the origin server requires the
  /// request to be conditional.
  /// </para><para>
  /// Its typical use is to avoid the "lost update" problem, where a client
  /// GETs a resource's state, modifies it, and PUTs it back to the server,
  /// when meanwhile a third party has modified the state on the server,
  /// leading to a conflict.  By requiring requests to be conditional, the
  /// server can assure that clients are working with the correct copies.
  /// </para><para>
  /// Responses using this status code SHOULD explain how to resubmit the
  /// request successfully.  For example:
  /// <code>
  /// HTTP/1.1 428 Precondition Required
  /// Content-Type: text/html
  ///
  /// &lt;html&gt;
  ///    &lt;head&gt;
  ///       &lt;title&gt;Precondition Required&lt;/title&gt;
  ///    &lt;/head&gt;
  ///    &lt;body&gt;
  ///       &lt;h1&gt;Precondition Required&lt;/h1&gt;
  ///       &lt;p&gt;This request is required to be conditional;
  ///       try using &quot;If-Match&quot;.&lt;/p&gt;
  ///    &lt;/body&gt;
  /// &lt;/html&gt;
  /// </code>
  /// </para><para>
  /// Responses with the 428 status code MUST NOT be stored by a cache.
  /// </para></summary>
  /// <remarks>
  /// https://tools.ietf.org/html/rfc6585
  /// </remarks>
  val PRECONDITION_REQUIRED : body:string -> WebPart

  /// <summary><para>
  /// 429
  /// </para><para>
  /// The user has sent too many requests in a given amount of time.
  /// Intended for use with rate limiting schemes.[18]
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// https://tools.ietf.org/html/rfc6585
  /// </remarks>
  val too_many_requests : bytes:byte [] -> WebPart
  
  /// <summary><para>
  /// 429
  /// </para><para>
  /// The user has sent too many requests in a given amount of time.
  /// Intended for use with rate limiting schemes.[18]
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// https://tools.ietf.org/html/rfc6585
  /// </remarks>
  val TOO_MANY_REQUESTS : body:string -> WebPart

/// 10.5 Server Error 5xx
/// Response status codes beginning with the digit "5" indicate cases in
/// which the server is aware that it has erred or is incapable of
/// performing the request. Except when responding to a HEAD request, the
/// server SHOULD include an entity containing an explanation of the
/// error situation, and whether it is a temporary or permanent
/// condition. User agents SHOULD display any included entity to the
/// user. These response codes are applicable to any request method.
module ServerErrors =

  /// <summary><para>
  /// 500
  /// </para><para>
  /// The server encountered an unexpected condition which prevented it
  /// from fulfilling the request.
  /// </para></summary>
  val internal_error : bytes:byte [] -> WebPart

  /// <summary><para>
  /// 500
  /// </para><para>
  /// The server encountered an unexpected condition which prevented it
  /// from fulfilling the request.
  /// </para></summary>
  val INTERNAL_ERROR : body:string -> WebPart

  /// An upstream server that suave communicated with did not respond in a timely fashion
  val bad_gateway : bytes:byte [] -> WebPart

  /// An upstream server that suave communicated with did not respond in a timely fashion
  val BAD_GATEWAY : body:string -> WebPart

  /// The service is currently under too much load and cannot service the request
  val service_unavailable : bytes:byte [] -> WebPart

  /// The service is currently under too much load and cannot service the request
  val SERVICE_UNAVAILABLE : body:string -> WebPart

  /// An upstream server that suave communicated with did not respond in a timely fashion
  val gateway_timeout : bytes:byte [] -> WebPart

  /// An upstream server that suave communicated with did not respond in a timely fashion
  val GATEWAY_TIMEOUT : body:string -> WebPart

  /// Only used internally in Suave.
  val invalid_http_version : bytes:byte [] -> WebPart

  /// Only used internally in Suave.
  val INVALID_HTTP_VERSION : WebPart

/// Module that deals with the applicatives of suave - use functions from this module
/// to filter what requests a given route responds to.
/// Functions have signature f :: params... -> HttpContext -> HttpContext option.
module Filters =

  open Suave.Logging

  /// Match on the path
  val path : pathAfterDomain:string -> WebPart

  /// Match on the initial path
  val pathStarts : pathAfterDomainSubstr:string -> WebPart

  /// Match on the method
  val ``method`` : ``method``:HttpMethod -> WebPart

  /// Match on the protocol being HTTPS
  val isSecure : WebPart

  /// Applies the regex to the path and matches on the result
  val pathRegex : pathAfterDomainRegex:string -> WebPart

  /// Match on the hostname (which is a required header for a Http client to send)
  /// -> allows you to have multiple sites with a single application.
  /// TODO: support SNI #177
  ///
  /// Perform a case-insensitive string comparison with context.request.clientHostTrustProxy
  /// which is the client-host; i.e. what the request says is the host, or
  /// what the proxy server says is the forwarded Host value. To match on what
  /// the web server *knows for sure* is the host, in the case when you've
  /// not overridden x-forwarded-host in your proxy, you should use
  /// x.request.host or Http.Applicatives.serverHost instead. The normal use-
  /// case, however, is to match on what's publically routable, which is the
  /// client host.
  val host : hostname:string -> WebPart

  /// This is the server's knowledge of what's the host is. In the case you
  /// have a clustered web server deployment, you might be more interested in
  /// what the client expects the host to be, since most validation logic you
  /// have in your app is concerned with this.
  val serverHost : hostname:string -> WebPart

  /// Alias for `host`.
  val clientHost : hostname:string -> WebPart

  /// <summary><para>
  /// The default log format for <see cref="log" />.  NCSA Common log format
  ///
  /// 127.0.0.1 user-identifier frank [10/Oct/2000:13:55:36 -0700] "GET /apache_pb.gif HTTP/1.0" 200 2326
  ///
  /// A "-" in a field indicates missing data.
  ///
  /// 127.0.0.1 is the IP address of the client (remote host) which made the request to the server.
  /// user-identifier is the RFC 1413 identity of the client.
  /// frank is the userid of the person requesting the document.
  /// [10/Oct/2000:13:55:36 -0700] is the date, time, and time zone when the server finished processing the request, by default in strftime format %d/%b/%Y:%H:%M:%S %z.
  /// "GET /apache_pb.gif HTTP/1.0" is the request line from the client. The method GET, /apache_pb.gif the resource requested, and HTTP/1.0 the HTTP protocol.
  /// 200 is the HTTP status code returned to the client. 2xx is a successful response, 3xx a redirection, 4xx a client error, and 5xx a server error.
  /// 2326 is the size of the object returned to the client, measured in bytes.
  /// </para></summary>
  val logFormat : ctx:HttpContext -> string

  /// <summary><para>
  /// Log the HttpRequest to the given logger, given the Suave Logger and a
  /// message formatter that can inspect the context and produce a message to
  /// send to the logger.
  /// </para></summary>
  val log : logger:Logger -> messageFun:(HttpContext -> string) -> WebPart

  /// <summary><para>
  /// Strongly typed route matching! Matching the uri can be used with the 'parsers'
  /// characters specified in Sscanf.
  /// </para><para>The supported characters for the formatter:</para><para>
  /// 'b', Boolean.Parse</para><para>
  /// 'd', int</para><para>
  /// 'i', int</para><para>
  /// 's', box</para><para>
  /// 'u', uint32</para><para>
  /// 'x', check (String.forall Char.IsLower) &gt;&gt; ((+) "0x") &gt;&gt; int</para><para>
  /// 'X', check (String.forall Char.IsUpper) &gt;&gt; ((+) "0x") &gt;&gt; int</para><para>
  /// 'o', ((+) "0o") &gt;&gt; int</para><para>
  /// 'e', float// no check for correct format for floats</para><para>
  /// 'E', float</para><para>
  /// 'f', float</para><para>
  /// 'F', float</para><para>
  /// 'g', float</para><para>
  /// 'G', float</para><para>
  /// 'M', parse_decimal</para><para>
  /// 'c', char
  /// </para></summary>
  val pathScan : pf:PrintfFormat<'a,'b,'c,'d,'t> -> h:('t -> WebPart) -> WebPart

  /// <summary> Fails the WebPart after x seconds</summary>
  val timeoutWebPart : timeout:System.TimeSpan -> child:WebPart -> WebPart

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
  val GET : WebPart

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
  val POST : WebPart

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
  /// </summary>
  val DELETE : WebPart

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
  val PUT : WebPart

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
  val HEAD : WebPart

  /// <summary><para>
  /// Match on CONNECT requests.
  /// </para><para>
  /// This specification (RFC 2616) reserves the method name CONNECT for use with a
  /// proxy that can dynamically switch to being a tunnel (e.g. SSL tunneling [44]).
  /// </para></summary>
  val CONNECT : WebPart

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
  val PATCH : WebPart

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
  val TRACE : WebPart

  /// Match on OPTIONS requests
  /// The OPTIONS method represents a request for information about the communication
  /// options available on the request/response chain identified by the Request-URI.
  /// This method allows the client to determine the options and/or requirements associated
  /// with a resource, or the capabilities of a server, without implying a resource
  /// action or initiating a resource retrieval.
  /// Responses to this method are not cacheable.
  val OPTIONS : WebPart

/// The files module can be used to serve from the file system. It encapsulates
/// common patterns like verifying that back-symlinks or keywords aren't used
/// to gain access outside the intended folder.
module Files =

  /// <summary><para>
  /// Send a file as a response to the request
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val sendFile : fileName:string -> compression:bool -> WebPart

  /// <summary><para>
  /// Send the embedded file by the filename given. Will search relative to the current assembly.
  /// Will also set the MIME type based on the file extension.
  /// </para></summary>
  val file : fileName:string -> WebPart

  /// <summary><para>
  /// Format a string with a local file path given a file name 'fileName'. You should
  /// use this helper method to find the current directory and concatenate that current
  /// directory to the filename which should be absolute and start with a path separator.
  /// </para></summary>
  /// <remarks>
  /// The current implementation doesn't take kindly to relative paths.
  /// </remarks>
  val resolvePath : rootPath:string -> fileName:string -> string


  /// <summary><para>
  /// 'browse' the file given as the filename, by sending it to the browser with a
  /// MIME-type/Content-Type header based on its extension. Will service from the
  /// rootPath.
  /// </para></summary>
  val browseFile : rootPath:string -> fileName:string -> WebPart

  /// <summary><para>
  /// 'browse' the file given as the filename, by sending it to the browser with a
  /// MIME-type/Content-Type header based on its extension. Will service from the
  /// current directory.
  /// </para></summary>
  val browseFileHome : fileName:string -> WebPart

  /// <summary><para>
  /// 'browse' the file in the sense that the contents of the file are sent based on the
  /// request's Url property. Will serve from the passed root path/directory.
  /// </para></summary>
  /// <remarks>
  /// The current implementation doesn't take kindly to relative paths.
  /// </remarks>
  val browse : rootPath:string -> WebPart

  /// <summary><para>
  /// 'browse' the file in the sense that the contents of the file are sent based on the
  /// request's Url property. Will serve from the current as configured in directory.
  /// Suave's runtime.
  /// </para></summary>
  val browseHome : WebPart

  /// <summary><para>
  /// Serve a 'file browser' for a rootPath
  /// </para></summary>
  /// <remarks>
  /// The current implementation doesn't take kindly to relative paths.
  /// </remarks>
  val dir : rootPath:string -> WebPart

  /// <summary><para>
  /// Serve a 'file browser' for the current directory
  /// </para></summary>
  val dirHome : WebPart

module Embedded =

  open System.Reflection

  /// <summary><para>
  /// Send an embedded resource as a response to the request. Allows you to control
  /// the source assembly to read from.
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val sendResource : source:Assembly -> resourceName:string -> compression:bool -> WebPart

  #if !NETSTANDARD1_5
  /// <summary><para>
  /// Send an embedded resource as a response to the request
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val sendResourceFromDefaultAssembly : resourceName:string -> compression:bool -> WebPart
  #endif

  /// <summary><para>
  /// Send the resource by the name given.
  /// Will also set the MIME type based on the file extension.
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val resource : source:Assembly -> name:string -> WebPart

  #if !NETSTANDARD1_5
  /// <summary><para>
  /// Send the resource by the name given.
  /// Will also set the MIME type based on the file extension.
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val resourceFromDefaultAssembly : name:string -> WebPart
  #endif

  /// <summary><para>
  /// 'browse' the file in the sense that the contents of the file are sent based on the
  /// request's Url property. Will serve from the executing assemblies resources.
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val browse : source:Assembly -> WebPart

  #if !NETSTANDARD1_5
  /// <summary><para>
  /// 'browse' the file in the sense that the contents of the file are sent based on the
  /// request's Url property. Will serve from the executing assemblies resources.
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val browseDefaultAsssembly : WebPart
  #endif

/// A module that implements the Server-Sent Event specification, which can be
/// read at www.w3.org/TR/eventsource.
module EventSource =

  /// Helper function that writes a string of data. Most often you are better
  /// off using the `send` function and give it a message, as this will ensure
  /// the proper framing is used. However, if you have a desire to write raw
  /// data, this function overrides the Socket.async_write function so that
  /// you will be writing UTF8 data only, as per the specification.
  val asyncWrite : out:Connection -> data:string -> SocketOp<unit>

  /// Same as `async_write`; convenience function.
  val (<<.) : out:Connection -> data:string -> SocketOp<unit>
    
  /// "If the line is empty (a blank line) - dispatch the event."
  /// Dispatches the event properly to the browser.
  val dispatch : out:Connection -> SocketOp<unit>
    
  /// "If the line starts with a U+003A COLON character (:) - Ignore the line."
  /// Writes a comment to the stream
  val comment : out:Connection -> cmt:string -> SocketOp<unit>

  /// "If the field name is 'event' - Set the event type buffer to field value."
  /// Writes the event type to the stream
  val eventType : out:Connection -> eventType:string -> SocketOp<unit>

  /// "If the field name is 'data' -
  /// Append the field value to the data buffer, then append a single
  /// U+000A LINE FEED (LF) character to the data buffer."
  /// Write a piece of data as part of the event
  val data : out:Connection -> data:string -> SocketOp<unit>

  /// "If the field name is 'id' - Set the last event ID buffer to the field value."
  /// Sets the last event id in the stream.
  val esId : out:Connection -> lastEventId:string -> SocketOp<unit>

  /// Sets the option for the EventSource instance, of how long to wait in ms
  /// until a new connection is spawned as a retry.
  val retry : out:Connection -> retry:uint32 -> SocketOp<unit>
    
  /// A container data type for the output events
  type Message =
    { id       : string
      data     : string
      ``type`` : string option }

  /// Create a new message to send over SSE
  val mkMessage : id:string -> data:string -> Message

  /// Create a new message with a given type to send over SSE
  val mkMessageType : id:string -> data:string -> typ:string -> Message

  /// send a message containing data to the output stream
  val send : out:Connection -> msg:Message -> SocketOp<unit>

  /// This function composes the passed function f with the hand-shake required
  /// to start a new event-stream protocol session with the browser.
  val handShake : fCont:(Connection -> SocketOp<Connection>) -> WebPart

module Control =

  /// <summary><para>
  /// This webpart closes the conection
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val CLOSE : WebPart

module CORS =

  open Utils

  [<RequireQualifiedAccess>]
  type InclusiveOption<'T> =
    | None
    | Some of 'T
    | All

  /// The configuration values for CORS
  type CORSConfig =
    {
      /// The list of allowed Uri(s) for requests.
      allowedUris             : InclusiveOption<string list>

      /// The list of allowed HttpMethods for the request.
      allowedMethods          : InclusiveOption<HttpMethod list>
      
      /// Allow cookies? This is sent in the AccessControlAllowCredentials header.
      allowCookies            : bool

      /// Should response headers be exposed to the client? This is sent in AccessControlExposeHeaders header. 
      exposeHeaders           : bool
      
      /// Max age in seconds the user agent is allowed to cache the result of the request.
      maxAge                  : int option }
    
    static member allowedUris_           : Property<CORSConfig, InclusiveOption<string list>>
    static member allowedMethods_        : Property<CORSConfig, InclusiveOption<HttpMethod list>>
    static member allowCookies_          : Property<CORSConfig, bool>
    static member exposeHeaders_         : Property<CORSConfig, bool>
    static member maxAge_                : Property<CORSConfig, int option>



  /// <summary><para>
  /// This WebPart handles CORS requests.
  /// </para><para>
  /// </para><para>
  /// </para></summary>
  /// <remarks>
  /// </remarks>
  val cors : CORSConfig:(CORSConfig) -> WebPart

  val defaultCORSConfig : CORSConfig