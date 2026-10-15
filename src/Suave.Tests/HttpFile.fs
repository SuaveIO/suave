module Suave.Tests.HttpFile

open Expecto

open System
open System.IO
open System.Net
open System.Net.Http
open System.Net.Http.Headers
open System.Text
open System.Net.Sockets

open Suave
open Suave.Successful
open Suave.Utils

open Suave.Tests.TestUtilities
open Suave.Testing

[<Tests>]
let ``canonicalization attacks`` (_: SuaveConfig) =
  testList "canonicalization attacks" [
    testCase "should throw" <| fun _ ->
      Expect.throwsT<Exception> (fun _ -> Files.resolvePath currentPath "../../passwd" |> ignore)
        "'../../passwd' is not a valid path"
    testCase "can use dot" <| fun _ ->
      Expect.equal (Files.resolvePath currentPath ".") currentPath "expect currentPath"

    testCase "sibling directory sharing the root's prefix is rejected" <| fun _ ->
      // root "<...>/app" must not serve "<...>/app-secret/passwd", even though
      // the latter starts with the root as a plain string prefix.
      let root = Path.Combine(currentPath, "app")
      Expect.throwsT<Exception> (fun _ -> Files.resolvePath root "../app-secret/passwd" |> ignore)
        "'../app-secret/passwd' escapes the root"

    testCase "sibling directory sharing the root's prefix is rejected for a rooted request path" <| fun _ ->
      // request paths start with '/', which is trimmed before combining
      let root = Path.Combine(currentPath, "app")
      Expect.throwsT<Exception> (fun _ -> Files.resolvePath root "/../app-secret/passwd" |> ignore)
        "'/../app-secret/passwd' escapes the root"

    testCase "a file directly below a prefix-sharing root is still served" <| fun _ ->
      let root = Path.Combine(currentPath, "app")
      let expected = Path.Combine(root, "index.html")
      Expect.equal (Files.resolvePath root "/index.html") expected
        "files inside the root must still resolve"

    testCase "root with a trailing separator resolves the same paths" <| fun _ ->
      let expected = Path.Combine(currentPath, "test-text-file.txt")
      let rootWithSeparator = currentPath + string Path.DirectorySeparatorChar
      Expect.equal (Files.resolvePath rootWithSeparator "/test-text-file.txt") expected
        "a trailing separator on the root should not matter"
      Expect.throwsT<Exception> (fun _ -> Files.resolvePath rootWithSeparator "../../passwd" |> ignore)
        "'../../passwd' is not a valid path"

    testCase "relative root is canonicalized" <| fun _ ->
      let expected = Path.Combine(currentPath, "test-text-file.txt")
      let relativeRoot = Path.Combine(currentPath, "sub", "..")
      Expect.equal (Files.resolvePath relativeRoot "/test-text-file.txt") expected
        "the root should be canonicalized before comparison"

    testCase "forward slashes are honoured on every platform" <| fun _ ->
      let expected = Path.Combine(currentPath, "sub", "test-text-file.txt")
      Expect.equal (Files.resolvePath currentPath "/sub/test-text-file.txt") expected
        "'/' should be translated to the platform separator"
      Expect.throwsT<Exception> (fun _ -> Files.resolvePath currentPath "/sub/../../passwd" |> ignore)
        "'..' segments written with '/' must still be caught"

    testCase "platform separator is honoured" <| fun _ ->
      let sep = string Path.DirectorySeparatorChar
      let expected = Path.Combine(currentPath, "sub", "test-text-file.txt")
      Expect.equal (Files.resolvePath currentPath (sep + "sub" + sep + "test-text-file.txt")) expected
        "expect the file below the root"
      Expect.throwsT<Exception> (fun _ ->
        Files.resolvePath currentPath (sep + ".." + sep + ".." + sep + "passwd") |> ignore)
        "'..' segments written with the platform separator must be caught"
  ]

[<Tests>]
let compression cfg =
  let runWithConfig = runWith cfg

  let testFileSize = (new FileInfo(Path.Combine(currentPath,"test-text-file.txt"))).Length

  testList "getting basic gzip/deflate responses" [
      testCase "200 OK returns 'Havana' with gzip " <| fun _ ->
        let actual = runWithConfig (OK "Havana") |> reqGZip HttpMethod.GET "/" None
        let expected = "Havana"
        Expect.equal actual expected "expecting 'Havana'"

      testCase "200 OK returns 'Havana' with deflate " <| fun _ ->
        let actual = runWithConfig (OK "Havana") |> reqDeflate HttpMethod.GET "/" None
        Expect.equal actual "Havana" "expecting 'Havana'"

      testCase "verifiying we get the same size uncompressed" <| fun _ ->
        Assert.Equal("length should match"
          , testFileSize
          , (runWithConfig (Files.browseFileHome "test-text-file.txt") |> reqBytes HttpMethod.GET "/" None).Length |> int64)

      testCase "gzip static file" <| fun _ ->
        Assert.Equal("length should match"
        , testFileSize
        , (runWithConfig (Files.browseFileHome "test-text-file.txt") |> reqGZipBytes HttpMethod.GET "/test-text-file-gzip" None).Length |> int64)
    ]

[<Tests>]
let ``compressed file cache`` cfg =
  let runWithConfig = runWith cfg

  // Big enough to be compressed (see Compression.MIN_BYTES_TO_COMPRESS) and
  // compressible enough that the two algorithms give different bytes.
  let expected = System.String.Join("\n", Array.replicate 100 "hello compressed world")

  /// Serve a file nothing else has served yet, so that the process-wide
  /// compressed-file cache starts out cold for it.
  let withTempFile f =
    let path = Path.Combine(Path.GetTempPath(), "suave-compression-" + Guid.NewGuid().ToString("N") + ".txt")
    File.WriteAllText(path, expected)
    try
      f path
    finally
      try File.Delete path with _ -> ()

  /// A single request against an already running server, asking for exactly
  /// one encoding and doing no decompression of its own.
  let request (encoding : string) (ctx : SuaveTestCtx) =
    use handler = createHandler DecompressionMethods.None None
    use client = createClient handler
    use message = createRequest HttpMethod.GET "/" "" None (endpointUri ctx.suaveConfig)
    message.Headers.TryAddWithoutValidation("Accept-Encoding", encoding) |> ignore
    use response = message |> send client (TimeSpan.FromSeconds 10.) ctx
    let contentEncoding = response.Content.Headers.ContentEncoding |> Seq.toList
    contentEncoding, contentByteArray response

  let decode encoding bytes =
    match encoding with
    | "gzip" -> Suave.Utils.Compression.gzipDecode bytes
    | "deflate" -> Suave.Utils.Compression.deflateDecode bytes
    | other -> Tests.failtestf "unexpected Content-Encoding '%s'" other

  let expectServedWith encoding (contentEncoding, bytes) =
    Expect.equal contentEncoding [ encoding ] (sprintf "should be served as '%s'" encoding)
    let decoded =
      try Encoding.UTF8.GetString (decode encoding bytes)
      with :? InvalidDataException ->
        Tests.failtestf "the body is not actually %s-compressed" encoding
    Expect.equal decoded expected (sprintf "the body should be the file, %s-compressed" encoding)

  // A file compressed for the first request must not be handed out - under a
  // different Content-Encoding - to the second one.
  let sequentially first second =
    withTempFile (fun path ->
      runWithConfig (Files.file path)
      |> withContext (fun ctx ->
        expectServedWith first (request first ctx)
        expectServedWith second (request second ctx)))

  testList "compressed file cache is keyed by algorithm" [
    testCase "gzip then deflate" <| fun _ -> sequentially "gzip" "deflate"
    testCase "deflate then gzip" <| fun _ -> sequentially "deflate" "gzip"
  ]

[<Tests>]
let ``obsolete compressed artifacts`` cfg =

  let expected = System.String.Join("\n", Array.replicate 100 "hello compressed world")
  let updated = System.String.Join("\n", Array.replicate 100 "hello recompressed world")

  /// A private root for the server's `_temporary_compressed_files` folder, so
  /// that counting what is in it cannot be confused by any other test.
  let withTempDir f =
    let dir = Path.Combine(Path.GetTempPath(), "suave-compression-" + Guid.NewGuid().ToString("N"))
    Directory.CreateDirectory dir |> ignore
    try
      f dir
    finally
      try Directory.Delete(dir, true) with _ -> ()

  let compressionFolder (root : string) =
    Path.Combine(root, "_temporary_compressed_files")

  let compressedFiles (root : string) =
    let folder = compressionFolder root
    if Directory.Exists folder then Directory.GetFiles folder else [||]

  /// Writes `content` and dates the file `age` in the past, so that a rewrite
  /// is unambiguously newer than the copy taken from it.
  let write (path : string) (content : string) (age : TimeSpan) =
    File.WriteAllText(path, content)
    File.SetLastWriteTime(path, DateTime.Now - age)

  let gzipRequest (ctx : SuaveTestCtx) =
    use handler = createHandler DecompressionMethods.None None
    use client = createClient handler
    use message = createRequest HttpMethod.GET "/" "" None (endpointUri ctx.suaveConfig)
    message.Headers.TryAddWithoutValidation("Accept-Encoding", "gzip") |> ignore
    use response = message |> send client (TimeSpan.FromSeconds 10.) ctx
    Encoding.UTF8.GetString (Suave.Utils.Compression.gzipDecode (contentByteArray response))

  testList "obsolete compressed artifacts are evicted" [
    testCase "recompressing a resource deletes the copy it supersedes" <| fun _ ->
      withTempDir (fun root ->
        let file = Path.Combine(root, "resource.txt")
        write file expected (TimeSpan.FromMinutes 10.)
        runWith { cfg with compressedFilesFolder = Some root } (Files.file file)
        |> withContext (fun ctx ->
          Expect.equal (gzipRequest ctx) expected "the file should be served gzipped"
          Expect.equal (compressedFiles root).Length 1 "there should be exactly one compressed copy"
          // A newer resource has to be compressed again; the copy that is
          // superseded must not be left behind in the folder.
          write file updated TimeSpan.Zero
          Expect.equal (gzipRequest ctx) updated "the new contents should be served"
          Expect.equal (compressedFiles root).Length 1 "the superseded copy should have been deleted"))

    testCase "a copy evicted from disk is compressed again on the next request" <| fun _ ->
      withTempDir (fun root ->
        let file = Path.Combine(root, "resource.txt")
        write file expected (TimeSpan.FromMinutes 10.)
        runWith { cfg with compressedFilesFolder = Some root } (Files.file file)
        |> withContext (fun ctx ->
          Expect.equal (gzipRequest ctx) expected "the file should be served gzipped"
          // an aggressive sweep: everything in the folder counts as obsolete
          Suave.Compression.cleanupFolder (compressionFolder root) TimeSpan.Zero 0 |> ignore
          Expect.equal (compressedFiles root).Length 0 "the sweep should have emptied the folder"
          Expect.equal (gzipRequest ctx) expected "the file should still be served after its copy was evicted"
          Expect.equal (compressedFiles root).Length 1 "the resource should have been compressed again"))

    testCase "cleanupFolder deletes files older than the maximum age" <| fun _ ->
      withTempDir (fun dir ->
        let old = Path.Combine(dir, "old")
        let recent = Path.Combine(dir, "recent")
        write old "old" (TimeSpan.FromHours 2.)
        write recent "recent" TimeSpan.Zero
        Suave.Compression.cleanupFolder dir (TimeSpan.FromHours 1.) 1000 |> ignore
        Expect.isFalse (File.Exists old) "the file over the maximum age should be gone"
        Expect.isTrue (File.Exists recent) "the recent file should be kept")

    testCase "cleanupFolder bounds the folder, oldest first" <| fun _ ->
      withTempDir (fun dir ->
        let files =
          [ for i in 1 .. 5 ->
              let path = Path.Combine(dir, sprintf "file-%d" i)
              write path (string i) (TimeSpan.FromMinutes (float (10 - i)))
              path ]
        Suave.Compression.cleanupFolder dir (TimeSpan.FromDays 365.) 2 |> ignore
        Expect.equal (files |> List.filter File.Exists) [ files.[3]; files.[4] ]
          "the two newest files should be the ones kept")

    testCase "a path that cannot be deleted takes one retry slot, not one per sweep" <| fun _ ->
      withTempDir (fun dir ->
        // A directory is a path `File.Delete` can never remove, on any
        // platform; it stands in for the file that refuses to go away.
        let stubborn = Path.Combine(dir, "stubborn")
        Directory.CreateDirectory stubborn |> ignore
        Expect.isFalse (Suave.Compression.tryDelete stubborn) "the path cannot be deleted"
        let queued = Suave.Compression.pendingDeletionCount ()
        // Offered again by hand, and met again by as many sweeps as we care
        // to run - each of which retries it and puts it back: it must still
        // hold exactly one slot in the bounded retry queue.
        Suave.Compression.tryDelete stubborn |> ignore
        Suave.Compression.cleanupFolder dir TimeSpan.Zero 0 |> ignore
        Suave.Compression.cleanupFolder dir TimeSpan.Zero 0 |> ignore
        Expect.equal (Suave.Compression.pendingDeletionCount ()) queued
          "the same path must not be queued more than once"
        // and once it is gone, it gives the slot back
        Directory.Delete stubborn
        Suave.Compression.cleanupFolder dir TimeSpan.Zero 0 |> ignore
        Expect.equal (Suave.Compression.pendingDeletionCount ()) (queued - 1)
          "a path that is finally gone should release its slot")

    testCase "the shutdown sweep runs once the server has stopped" <| fun _ ->
      withTempDir (fun root ->
        let file = Path.Combine(root, "resource.txt")
        write file expected (TimeSpan.FromMinutes 10.)
        // with room for no files at all, every artifact counts as obsolete,
        // so the sweep at shutdown should empty the folder
        let maxFiles = Suave.Compression.MAX_COMPRESSED_FILES
        Suave.Compression.MAX_COMPRESSED_FILES <- 0
        let ctx = runWith { cfg with compressedFilesFolder = Some root } (Files.file file)
        try
          Expect.equal (gzipRequest ctx) expected "the file should be served gzipped"
          Expect.equal (compressedFiles root).Length 1 "there should be one compressed copy to sweep"
          // The sweep belongs to the server's own shutdown, so awaiting the
          // server task is enough - there is nothing left to run after it.
          ctx.cts.Cancel()
          let stopped =
            try ctx.serverTask.Wait(TimeSpan.FromSeconds 10.)
            with :? AggregateException -> true
          Expect.isTrue stopped "the server should have shut down"
          Expect.equal (compressedFiles root).Length 0
            "the artifact should have been swept once the server stopped"
        finally
          Suave.Compression.MAX_COMPRESSED_FILES <- maxFiles
          disposeContext ctx)

    testCase "cleanupFolder tolerates a folder that does not exist" <| fun _ ->
      let missing = Path.Combine(Path.GetTempPath(), "suave-compression-" + Guid.NewGuid().ToString("N"))
      // there is nothing to delete, and nothing to throw either
      Suave.Compression.cleanupFolder missing TimeSpan.Zero 0 |> ignore
      Expect.isFalse (Directory.Exists missing) "the sweep should not create the folder"
  ]

[<Tests>]
let ``http HEAD method`` cfg =
  let runWithConfig = runWith cfg
  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    string binding.socketBinding.ip,
    int binding.socketBinding.port

  testList "HEAD on `file`" [
    testCase  "HEAD does not return content" <| fun _ ->

      let ctx = runWithConfig (Files.browseFileHome "test-text-file.txt")

      withContext (fun _ ->
        let client = new TcpClient(ip, port)
        let message = sprintf "HEAD %s HTTP/1.1\r\nHost: %s\r\nConnection: Close\r\n\r\n" "/foo" ip
        let outputData = ASCII.bytes message
        let stream = client.GetStream()
        stream.Write(outputData, 0, outputData.Length)

        use streamReader = new StreamReader(stream)

        // read header lines
        let rec loop _ =
          let line = streamReader.ReadLine()
          if line.Equals("") then ()
          else loop ()
        loop ()

        Expect.equal streamReader.EndOfStream true "Stream should be at the end.") ctx
  ]

[<Tests>]
let ``ETag support`` cfg =
  let runWithConfig = runWith cfg

  let expectedEtag = Files.fileEtag (Path.Combine(currentPath, "test-text-file.txt"))

  testList "ETag header on static files" [
    testCase "ETag header is present on file responses" <| fun _ ->
      let headers =
        runWithConfig (Files.browseFileHome "test-text-file.txt")
        |> reqHeaders HttpMethod.GET "/" None
      let etagValues =
        match headers.TryGetValues("ETag") with
        | true, values -> values |> Seq.toList
        | false, _ -> []
      Expect.equal etagValues [expectedEtag] "ETag value should be a weak validator derived from file metadata"

    testCase "fileEtag is stable for an unchanged file" <| fun _ ->
      let a = Files.fileEtag (Path.Combine(currentPath, "test-text-file.txt"))
      let b = Files.fileEtag (Path.Combine(currentPath, "test-text-file.txt"))
      Expect.equal a b "Two consecutive calls should return the same ETag"
      Expect.stringStarts a "W/\"" "ETag should be a weak validator"

    testCase "missing file falls through to the next WebPart" <| fun _ ->
      let app =
        choose [
          Files.browseHome
          RequestErrors.NOT_FOUND "fallback"
        ]
      let status, body =
        runWithConfig app
        |> reqResp HttpMethod.GET "/file-that-does-not-exist.txt" "" None None DecompressionMethods.None id (fun response ->
             response.StatusCode,
             contentString response)
      Expect.equal status HttpStatusCode.NotFound "Should use the fallback response"
      Expect.equal body "fallback" "Should return the fallback body"

    testCase "If-None-Match with matching ETag returns 304 Not Modified" <| fun _ ->
      let setIfNoneMatch (req : HttpRequestMessage) =
        req.Headers.TryAddWithoutValidation("If-None-Match", expectedEtag) |> ignore
        req
      let status =
        runWithConfig (Files.browseFileHome "test-text-file.txt")
        |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None setIfNoneMatch statusCode
      Expect.equal status HttpStatusCode.NotModified "Should be 304 Not Modified"

    testCase "304 responses include an ETag header" <| fun _ ->
      let setIfNoneMatch (req : HttpRequestMessage) =
        req.Headers.TryAddWithoutValidation("If-None-Match", expectedEtag) |> ignore
        req
      let headers =
        runWithConfig (Files.browseFileHome "test-text-file.txt")
        |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None setIfNoneMatch responseHeaders
      let etagValues =
        match headers.TryGetValues("ETag") with
        | true, values -> values |> Seq.toList
        | false, _ -> []
      Expect.equal etagValues [expectedEtag] "ETag on 304 response should equal the request's If-None-Match"

    testCase "If-None-Match with non-matching ETag returns 200 OK" <| fun _ ->
      let setIfNoneMatch (req : HttpRequestMessage) =
        req.Headers.TryAddWithoutValidation("If-None-Match", "W/\"stale-etag\"") |> ignore
        req
      let status =
        runWithConfig (Files.browseFileHome "test-text-file.txt")
        |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None setIfNoneMatch statusCode
      Expect.equal status HttpStatusCode.OK "Should be 200 OK when the ETag does not match"

    testCase "If-None-Match with wildcard returns 304 Not Modified" <| fun _ ->
      let setIfNoneMatch (req : HttpRequestMessage) =
        req.Headers.TryAddWithoutValidation("If-None-Match", "*") |> ignore
        req
      let status =
        runWithConfig (Files.browseFileHome "test-text-file.txt")
        |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None setIfNoneMatch statusCode
      Expect.equal status HttpStatusCode.NotModified "Wildcard If-None-Match should match any existing resource"
  ]


[<Tests>]
let ``byte ranges`` cfg =
  let runWithConfig = runWith cfg

  /// Serves a fresh "0123456789" file, so the compressed-file cache never has
  /// an entry for it.
  let withDigitsFile f =
    let path = Path.Combine(Path.GetTempPath(), "suave-range-" + Guid.NewGuid().ToString("N") + ".txt")
    File.WriteAllText(path, "0123456789")
    try f path
    finally try File.Delete path with _ -> ()

  let contentRange (response : HttpResponseMessage) =
    match response.Content.Headers.TryGetValues "Content-Range" with
    | true, values -> Some (String.Join(",", values))
    | _ -> None

  /// Status, Content-Range, Content-Length and body of a request with `headers`.
  let get (headers : (string * string) list) path =
    let setHeaders (r : HttpRequestMessage) =
      for (k, v) in headers do r.Headers.TryAddWithoutValidation(k, v) |> ignore
      r
    runWithConfig (Files.file path)
    |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None setHeaders (fun response ->
         response.StatusCode,
         contentRange response,
         (let l = response.Content.Headers.ContentLength in if l.HasValue then Some l.Value else None),
         contentString response)

  let ranged range expectedRange (expectedBody : string) =
    withDigitsFile (fun path ->
      let status, cr, length, body = get [ "Range", range ] path
      Expect.equal status HttpStatusCode.PartialContent "should be 206"
      Expect.equal cr (Some expectedRange) "Content-Range"
      Expect.equal length (Some (int64 expectedBody.Length)) "Content-Length"
      Expect.equal body expectedBody "body")

  let full range =
    withDigitsFile (fun path ->
      let status, cr, _, body = get [ "Range", range ] path
      Expect.equal status HttpStatusCode.OK "should be 200"
      Expect.isNone cr "a 200 carries no Content-Range"
      Expect.equal body "0123456789" "the whole file")

  let unsatisfiable range =
    withDigitsFile (fun path ->
      let status, cr, _, body = get [ "Range", range ] path
      Expect.equal (int status) 416 "should be 416"
      Expect.equal cr (Some "bytes */10") "Content-Range of a 416"
      Expect.equal body "" "no body")

  testList "byte ranges on `file`" [
    testCase "a closed range is inclusive" <| fun _ -> ranged "bytes=2-5" "bytes 2-5/10" "2345"
    testCase "a single byte" <| fun _ -> ranged "bytes=0-0" "bytes 0-0/10" "0"
    testCase "an open-ended range runs to the end" <| fun _ -> ranged "bytes=5-" "bytes 5-9/10" "56789"
    testCase "a suffix range is the last n bytes" <| fun _ -> ranged "bytes=-3" "bytes 7-9/10" "789"
    testCase "a suffix longer than the file is the whole file" <| fun _ -> ranged "bytes=-30" "bytes 0-9/10" "0123456789"
    testCase "a range past the end is clamped" <| fun _ -> ranged "bytes=8-20" "bytes 8-9/10" "89"
    testCase "a range starting at the length is 416" <| fun _ -> unsatisfiable "bytes=10-"
    testCase "a range starting past the end is 416" <| fun _ -> unsatisfiable "bytes=20-30"
    testCase "an empty suffix is 416" <| fun _ -> unsatisfiable "bytes=-0"
    testCase "an inverted range is ignored" <| fun _ -> full "bytes=5-2"
    testCase "a malformed range is ignored" <| fun _ -> full "bytes=abc"
    testCase "an unknown unit is ignored" <| fun _ -> full "items=0-1"
    testCase "multiple ranges are answered with the whole file" <| fun _ -> full "bytes=0-1,4-5"

    testCase "a full response carries no Content-Range" <| fun _ ->
      withDigitsFile (fun path ->
        let status, cr, length, body = get [] path
        Expect.equal status HttpStatusCode.OK "should be 200"
        Expect.isNone cr "no Content-Range on a 200"
        Expect.equal length (Some 10L) "Content-Length"
        Expect.equal body "0123456789" "the whole file")

    testCase "a compressed response carries no Content-Range" <| fun _ ->
      let headers =
        runWithConfig (Files.browseFileHome "test-text-file.txt")
        |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None
             (fun r -> r.Headers.TryAddWithoutValidation("Accept-Encoding", "gzip") |> ignore; r)
             (fun response -> response.Content.Headers.ContentEncoding |> Seq.toList, contentRange response)
      Expect.equal (fst headers) [ "gzip" ] "should be compressed"
      Expect.isNone (snd headers) "no Content-Range on a compressed 200"

    testCase "a range request is served uncompressed" <| fun _ ->
      let path = Path.Combine(currentPath, "test-text-file.txt")
      let expected = File.ReadAllBytes path
      let encoding, cr, bytes =
        runWithConfig (Files.file path)
        |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None
             (fun r ->
                r.Headers.TryAddWithoutValidation("Accept-Encoding", "gzip") |> ignore
                r.Headers.TryAddWithoutValidation("Range", "bytes=0-9") |> ignore
                r)
             (fun response ->
                response.Content.Headers.ContentEncoding |> Seq.toList,
                contentRange response,
                contentByteArray response)
      Expect.equal encoding [] "a 206 is not compressed"
      Expect.equal cr (Some (sprintf "bytes 0-9/%d" expected.Length)) "Content-Range"
      Expect.equal bytes expected.[0..9] "the first ten bytes"
  ]

[<Tests>]
let ``paths outside the root`` cfg =
  let runWithConfig = runWith cfg
  let ip, port =
    let binding = SuaveConfig.firstBinding cfg
    string binding.socketBinding.ip,
    int binding.socketBinding.port

  /// A root folder with a secret file next to it, in its parent.
  let withRoot f =
    let parent = Path.Combine(Path.GetTempPath(), "suave-root-" + Guid.NewGuid().ToString("N"))
    let root = Path.Combine(parent, "public")
    Directory.CreateDirectory root |> ignore
    File.WriteAllText(Path.Combine(parent, "secret.txt"), "secret")
    File.WriteAllText(Path.Combine(root, "index.txt"), "public")
    try f root
    finally try Directory.Delete(parent, true) with _ -> ()

  let run (webPart : WebPart) rawPath =
    let ctx =
      { HttpContext.empty with
          request = { HttpRequest.empty with rawPath = rawPath }
          runtime = { HttpContext.empty.runtime with mimeTypesMap = Writers.defaultMimeTypesMap } }
    webPart ctx |> Async.RunSynchronously

  /// The status line of a raw request for `rawPath`, sent without the client
  /// normalising the path away.
  let statusLine (rawPath : string) (ctx : SuaveTestCtx) =
    withContext (fun _ ->
      use client = new TcpClient(ip, port)
      let message = sprintf "GET %s HTTP/1.1\r\nHost: %s\r\nConnection: Close\r\n\r\n" rawPath ip
      let outputData = ASCII.bytes message
      let stream = client.GetStream()
      stream.Write(outputData, 0, outputData.Length)
      use streamReader = new StreamReader(stream)
      streamReader.ReadLine()) ctx

  testList "paths outside the root are declined" [
    testCase "tryResolvePath answers None outside the root" <| fun _ ->
      withRoot (fun root ->
        Expect.isNone (Files.tryResolvePath root "/../secret.txt") "outside the root"
        Expect.equal (Files.tryResolvePath root "/index.txt") (Some (Path.Combine(Path.GetFullPath root, "index.txt"))) "inside the root")

    testCase "browse declines" <| fun _ ->
      withRoot (fun root -> Expect.isNone (run (Files.browse root) "/../secret.txt") "declined")

    testCase "browseFile declines" <| fun _ ->
      withRoot (fun root -> Expect.isNone (run (Files.browseFile root "../secret.txt") "/") "declined")

    testCase "dir declines" <| fun _ ->
      withRoot (fun root -> Expect.isNone (run (Files.dir root) "/..") "declined")

    testCase "browse still serves files inside the root" <| fun _ ->
      withRoot (fun root -> Expect.isSome (run (Files.browse root) "/index.txt") "served")

    testCase "a traversal request falls through to the next web part" <| fun _ ->
      withRoot (fun root ->
        let app = choose [ Files.browse root; RequestErrors.NOT_FOUND "nope" ]
        let line = runWithConfig app |> statusLine "/../secret.txt"
        Expect.equal line "HTTP/1.1 404 Not Found" "should be the fallback 404, not a 500")
  ]

[<Tests>]
let ``unwritable compression folder`` cfg =
  testList "unwritable compression folder" [
    testCase "a compressible file is served uncompressed" <| fun _ ->
      // A folder below a regular file can never be created.
      let blocker = Path.GetTempFileName()
      // A file nothing else has served, so the process-wide compressed-file
      // cache cannot already hold a copy of it in some other folder.
      let path = Path.Combine(Path.GetTempPath(), "suave-uncompressed-" + Guid.NewGuid().ToString("N") + ".txt")
      File.WriteAllText(path, String.replicate 200 "compress me ")
      try
        let cfg = { cfg with compressedFilesFolder = Some (Path.Combine(blocker, "sub")) }
        let encoding, bytes =
          runWith cfg (Files.file path)
          |> reqResp HttpMethod.GET "/" "" None None DecompressionMethods.None
               (fun r -> r.Headers.TryAddWithoutValidation("Accept-Encoding", "gzip") |> ignore; r)
               (fun response -> response.Content.Headers.ContentEncoding |> Seq.toList, contentByteArray response)
        Expect.equal encoding [] "served without Content-Encoding"
        Expect.equal bytes (File.ReadAllBytes path) "the file as it is"
      finally
        try File.Delete blocker with _ -> ()
        try File.Delete path with _ -> ()
  ]