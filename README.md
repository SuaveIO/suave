# Introduction

![Suave Logo](https://raw.githubusercontent.com/SuaveIO/resources/master/images/suave1.png)

Suave is a simple web development F# library providing a lightweight web server
and a set of combinators to manipulate route flow and task composition. Suave
is inspired in the simplicity of Happstack and born out of the necessity of
embedding web server capabilities in my own applications. Suave supports full
HTTP/2, Server-Sent Events, WebSockets, HTTPS/TLS, multiple TCP/IP bindings
(with SO_REUSEPORT on Linux/BSD), rate limiting, security headers, validation
web parts, the AOT-friendly `Router` module, Basic Access Authentication and
Keep-Alive.

Suave performs non-blocking IO throughout: its internals were rewritten around
Task, Memory and Pipelines targeting .NET 9 and later, focusing on fewer
allocations, higher throughput and lower latency.

## Build Status

| Platform | Status         |
| -------- | -------------- |
| Linux     | [![Build status](https://github.com/SuaveIO/suave/actions/workflows/build-suave.yml/badge.svg)](https://github.com/SuaveIO/suave/actions/workflows/build-suave.yml) |

What follows is a tutorial on how to create applications. Scroll past the
tutorial to see detailed function documentation.

# Tutorial: Hello World!

The simplest Suave application is a simple HTTP server that greets all visitors
with the string `"Hello World!"`

``` fsharp
open Suave

startWebServer defaultConfig (Successful.OK "Hello World!")
```

Now that you've discovered how to do "Hello World!", go read the
[documentation](https://suave.io/) — guides and recipes live in
[`website/content/docs`](website/content/docs); the API reference is generated
from [`docs-api/`](docs-api) with
[FSharp.Formatting](https://fsprojects.github.io/FSharp.Formatting/) into
`website/content/reference` via `./scripts/generate-api-docs.sh` (also FAKE
target `Docs`).

# How to Build

To execute the build script, invoke following command on the Linux or MacOs console:

```
./build.sh
```

Or in the Microsoft Windows MSDOS console:

```
build
```

# Coding Guidelines

Suave.X where X is a module is where we expect users to look. We don't expect users
of the library to have to look at Y in Suave.X.Y, so for server-specific code, please
stick to the Y module/namespace. That way we make the API discoverable.


## Style Guide

Two space indentation.

``` fsharp
match x with // '|' characters at base of 'match'
| A     -> ()
| Bcdef -> "aligned arrows" // space after '|' character
```

Parameters

Let type annotations be specified with spaces after the argument symbol and before
the type.

``` fsharp
module MyType =
  let ofString (scheme : string) =
    // ...
```

Method formatting with no spaces after/before normal parenthesis

``` fsharp
let myMethodName firstArg (second : WithType) = async { // and monad builder
  return! f firstArg second
  } // at base of 'let' + 2 spaces
```

You need to document your methods with '///' to create inline documentation. This documentation
is used for two purposes. First, to automatically generate on-line API documentation. Second, to
generate an XML documentation file to be included in the NuGet package, so that users of the library
can understand the intention behind a method easily.

Don't put unnecessary parenthesis unless it makes the code more clear.

When writing functions that take some sort of 'configuration' or that you can
imagine would like to be called with a parameter which is almost always the same
value for another function body's call-site, put that parameter before
more-often-varying parameters in the function signature.
