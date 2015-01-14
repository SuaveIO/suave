module Suave.Json

open System.IO

open Newtonsoft.Json

open System.Text

open Suave.Http
open Suave.Web

let try_parse_json (r:Types.HttpRequest) =
  let str = Encoding.UTF8.GetString(r.raw_form);
  try
    Some (JsonConvert.DeserializeObject<'a>(str)), str
  with
  | e -> None, str

let json_success json =
  ((Successful.OK (JsonConvert.SerializeObject(json))) >>= (Writers.set_mime_type "application/json"))

let json_parse_fail str =
  RequestErrors.BAD_REQUEST ("Could not parse JSON: " + str)

let post_data_parse_fail =
  RequestErrors.BAD_REQUEST "Unable to parse post data"

/// Expose function f through a json call; lets you write like
///
/// let app =
///   url "/path"  >>= map_json some_function;
///
let map_json' f =
  Types.request(fun r ->
    match try_parse_json r with
    | Some requestJson, _ -> json_success(f requestJson)
    | None, str -> json_parse_fail str)

let map_json_async' (f: 'a -> Async<'b>): Types.WebPart =
  fun httpContext ->
    async {
      match try_parse_json httpContext.request with
      | Some requestJson, _ ->
        let! response = f requestJson
        let responseJson = JsonConvert.SerializeObject(response)
        return! json_success responseJson httpContext
      | None, str -> return! json_parse_fail str httpContext
  }

/// Expose function f through an async json call
let map_json_async (f: 'a -> Async<'b>) : Types.WebPart =
  choose
    [
      ParsingAndControl.parse_post_data >>= map_json_async' f
      post_data_parse_fail
    ]

let map_json (f: 'a -> 'b) : Types.WebPart =
  choose
    [
      ParsingAndControl.parse_post_data >>= map_json' f
      post_data_parse_fail
    ]