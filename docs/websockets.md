---
layout: default
---

Realtime Messages With WebSockets
---------------------------------

It's easy to set up WebSockets with Suave.

First, define a function that takes `WebSocket` and `HttpContext` typed parameters, and returns a socket computation expression:

{% highlight fsharp %}
open Suave.Sockets
open Suave.Sockets.Control
open Suave.WebSocket

let ws (webSocket : WebSocket) (context: HttpContext) =
    socket {
      ...
    }
{% endhighlight %}

Next, use the `read` and `send` function to receive and send messages to the clients:

{% highlight fsharp %}
    socket {
        let mutable loop = true

        while loop do
            let! msg = webSocket.read()

            match msg with
            | (Text, data, true) ->
                let str = UTF8.toString data
                let response = sprintf "response to %s" str
                let byteResponse =
                    response
                    |> System.Text.Encoding.ASCII.GetBytes
                    |> ByteSegment
                do! webSocket.send Text byteResponse true

            | (Close, _, _) ->
                let emptyResponse = [||] |> ByteSegment
                do! webSocket.send Close emptyResponse true
                loop <- false

            | _ -> ()
      }
{% endhighlight %}

Then, use the `handShake` function to fit it in your web server:

{% highlight fsharp %}
let app : WebPart =
    choose [
      path "/websocket" >=> handShake ws
      GET >=> choose [ path "/" >=> file "index.html"; browseHome ]
      NOT_FOUND "Found no handlers." ]
{% endhighlight %}

The complete example can be found [here](https://github.com/SuaveIO/suave/tree/master/examples/WebSocket).