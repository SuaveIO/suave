---
layout: default
---

## Serving static files, HTTP Compression and MIME types

Suave supports **gzip** and **deflate** http compression encodings. Http
compression is configured via the MIME types map in the server configuration
record. By default Suave does not serve files with extensions not registered in
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
    >=> (function | ".avi" -> mkMimeType "video/avi" false | _ -> None)

let webConfig = { defaultConfig with mimeTypesMap = mimeTypes }
{% endhighlight %}