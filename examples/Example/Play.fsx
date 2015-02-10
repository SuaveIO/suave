#r "bin/Release/suave.dll"

open Suave
open Suave.Http

open Successful

let app : HttpPart =
  choose [
    Applicatives.POST
      >>= ((Applicatives.url "/special") >=> (Applicatives.url_regex @"^/api/\w/.*$"))
      >>= (OK "yup")
    Redirection.MOVED_PERMANENTLY "http://haf.github.io"
    ]