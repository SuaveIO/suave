module Suave.Router

open System.Collections.Generic
open Suave

type RouteBag = Dictionary<string*HttpMethod, WebPart>

let register (routes:RouteBag) (path:string) (method:HttpMethod) (part:WebPart) =
  routes.Add((path,method),part)

let route (routes:RouteBag) (ctx:HttpContext) =
  if routes.ContainsKey(ctx.request.path,ctx.request.method) then
    let part = routes.[ctx.request.path,ctx.request.method]
    part ctx
  else
    async.Return(None)

let router (routes:RouteBag) (xxs:((HttpMethod*string)*(WebPart)) list) =
  List.iter (fun ((method,path),p) -> register routes path method p) xxs

let GET (routes:RouteBag) (xxs:(string*(WebPart)) list) =
  List.iter (fun (path,p) -> register routes path HttpMethod.GET p) xxs

let POST (routes:RouteBag) (xxs:(string*(WebPart)) list) =
  List.iter (fun (path,p) -> register routes path HttpMethod.POST p) xxs


let sample_app : WebPart =
  let routes = new RouteBag()
  router routes [ 
    (HttpMethod.GET, "/hello") , Successful.OK "Hello world!" ;
    (HttpMethod.GET, "/add")   , Successful.OK "Hello world!" ;
    (HttpMethod.GET, "/delete"), Successful.OK "Hello world!" ;
  ]
  GET routes [ 
    "/hello" , Successful.OK "Hello world!" ;
    "/add"   , Successful.OK "Hello world!" ;
    "/delete", Successful.OK "Hello world!" ;
  ]
  route routes

