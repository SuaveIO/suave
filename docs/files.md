---
layout: default
---

## Serving static files, HTTP Compression and MIME types

A typical static file serving Suave application would look somewhat like this:

{% highlight fsharp %}
let app : WebPart =
  choose [
    Filters.GET >=> choose [ Filters.path "/" >=> Files.file "index.html"; Files.browseHome ]
    RequestErrors.NOT_FOUND "Page not found." 
    ]
{% endhighlight %}

The main file combinators are `file`, `browseHome` and variations of these. To learn about all of them check out the documentation here https://suave.io/Suave.html#def:module%20Suave.Files

`file` will take the relative or absolute path for the file we want to serve to the client. It will set MIME-type headers based on the file extension.

`browseHome` will match existing files in the `homeFolder` based on the `Url` property and will serve them via the `file` combinator; `homeFolder` is a configuration parameter and can be set in the configuration record.

{% highlight fsharp %}
startWebServer { defaultConfig with homeFolder = Some @"C:\MyFiles" } app
{% endhighlight %}

Suave supports **gzip** and **deflate** http compression encodings. Http
compression is configured via the MIME types map in the server configuration
record. 

By default Suave does not serve files with extensions not registered in
the mime types map.

The default mime types map `defaultMimeTypesMap` looks like this.

{% highlight fsharp %}
let defaultMimeTypesMap = function
  | ".css" -> mkMimeType "text/css" true
  | ".gif" -> mkMimeType "image/gif" false
  | ".png" -> mkMimeType "image/png" false
  | ".htm"
  | ".html" -> mkMimeType "text/html" true
  | ".jpe"
  | ".jpeg"
  | ".jpg" -> mkMimeType "image/jpeg" false
  | ".js"  -> mkMimeType "application/x-javascript" true
  | _      -> None
{% endhighlight %}

You can register additional MIME extensions by creating a new mime map in the following fashion.

{% highlight fsharp %}
// Adds a new mime type to the default map
let mimeTypes =
  defaultMimeTypesMap
    @@ (function | ".avi" -> mkMimeType "video/avi" false | _ -> None)

let webConfig = { defaultConfig with mimeTypesMap = mimeTypes }
{% endhighlight %}