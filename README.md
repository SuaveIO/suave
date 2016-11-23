# Introduction

![Suave Logo](https://raw.githubusercontent.com/SuaveIO/resources/master/images/suave1.png)

Suave is a simple web development F# library providing a lightweight web server
and a set of combinators to manipulate route flow and task composition. Suave
is inspired in the simplicity of Happstack and born out of the necessity of
embedding web server capabilities in my own applications.  Still in its early
stages Suave supports Websocket, HTTPS, multiple TCP/IP bindings, Basic Access
Authentication, Keep-Alive.

Suave also takes advantage of F# asynchronous
workflows to perform non-blocking IO. In fact, Suave is written in a completely
non-blocking fashion throughout.

Sponsored by
[qvitoo – A.I. bookkeeping and Suave in production since many years](https://qvitoo.com/?utm_source=github&utm_campaign=suave).

## Build Status

| Platform | Status         |
| -------- | -------------- |
| Windows  | [![Build status](https://ci.appveyor.com/api/projects/status/g0lum9qndwb018sy/branch/master?svg=true)](https://ci.appveyor.com/project/AdemarGonzalez/suave/branch/master) |
| Mono/Linux     | [![Build status](https://travis-ci.org/SuaveIO/suave.svg?branch=master)](https://travis-ci.org/SuaveIO/suave) |

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
[rest of the documentation](https://suave.io/) – editable in the `docs` folder.

# Suave.Testing

We have a NuGet ready for your testing needs; Suave is an excellent server for
running in-process integation tests, as it's very fast to spawn. On an ordinary
laptop, running hundreds of randomised tests and micro-benchmarks as well as all
Suave unit tests, take about 5 seconds on mono.

Start by installing:

```
paket add nuget suave.testing
```

You can now use it:

``` fsharp
open Suave
open Suave.Testing
open Expecto

testCase "parsing a large multipart form" <| fun _ ->

  let res =
    runWithConfig (OK "hi")
    |> req HttpMethod.POST "/" (Some byteArrayContent)

  Expect.equal res "hi" "Should get the correct result"
```

All of our tests use this assembly; you can do too.

# How to Build

We're using a cross-platform build suite called
[albacore](https://github.com/Albacore/albacore)/[rake](https://github.com/ruby/rake).

Or build using:

```bash
./build.sh
```

Or

```cmd
build.cmd
```

## Build prerequisites

* [Ruby](https://www.ruby-lang.org/en/documentation/installation/)
* [RubyGems](https://rubygems.org/pages/download)
* [Bundler](http://bundler.io/)
* [Libuv](https://github.com/libuv/libuv)

On Linux and OS X/macOS you should have ruby out of the box (you might need to install bundler though). 
On Windows, you have to [install it](https://github.com/albacore/albacore/#installing-ruby-on-windows).

To execute the build script, invoke following commands:

```
bundle
bundle exec rake
```

After you managed to build with the rake script, you can also use `./build.sh` on any OS or `build.cmd` on Windows which only compile solution and run tests.

## Libuv installation

On OS X:

    brew install libuv --universal
    and then `export LD_LIBRARY_PATH=/usr/local/lib:/usr/lib:/lib`

On Windows:

    @powershell -NoProfile -ExecutionPolicy Bypass -Command "Start-FileDownload 'https://github.com/libuv/libuv/archive/v1.7.5.zip'"
    7z x v1.7.5.zip & cd libuv-1.7.5 & vcbuild.bat x86 shared debug
    mkdir src\\Suave.Tests\\bin\\Release\\ & cp libuv-1.7.5\\Debug\\libuv.dll src\\Suave.Tests\\bin\\Release\\libuv.dll

On Linux Ubuntu/Debian:

    sudo apt-get install automake libtool
    curl -sSL https://github.com/libuv/libuv/archive/v1.7.5.tar.gz | sudo tar zxfv - -C /usr/local/src
    cd /usr/local/src/libuv-1.7.5
    sudo sh autogen.sh
    sudo ./configure
    sudo make
    sudo make install
    sudo rm -rf /usr/local/src/libuv-1.7.5 && cd ~/
    sudo ldconfig

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

You need to document your methods with '///' to create XML-doc. A XML
documentation file is generated together with the compilation and is distributed
with the NuGet so that others can read your code's intentions easily.

Don't put unnecessary parenthesis unless it makes the code more clear.

When writing functions that take some sort of 'configuration' or that you can
imagine would like to be called with a parameter which is almost always the same
value for another function body's call-site, put that parameter before
more-often-varying parameters in the function signature.

## Testing

Run Tests as a console app. Return status code = 0 means success.

## Upgrade openssl

Windows: paket update openssl.redist

OS X: brew install openssl && brew update openssl && cp /usr/local/Cellar/openssl/1.0.1j_1/lib/ .

Linux: ...

# Community

## Chat Room

We have a chat room in case you feel like chatting a bit. 

[![Chat Room](https://badges.gitter.im/SuaveIO/suave.png)](https://gitter.im/SuaveIO/suave)

## Integrations

 * https://github.com/rayokota/generator-angular-suave
 * [FsReveal](https://github.com/fsprojects/FsReveal)
 * [TodoBackendSuave](https://github.com/JonCanning/TodoBackendSuave)
