namespace Suave.Utils

[<RequireQualifiedAccess>]
module Parse =
  open System
  open System.Globalization

  let private parseUsing<'T> (f:string -> bool * 'T) s =
    match f s with
    | true, i -> Choice1Of2 i
    | false, _ -> Choice2Of2 (sprintf "Cound not parse '%s' to %s" s typeof<'T>.Name)

  let int32 = parseUsing Int32.TryParse
  let uint32 = parseUsing UInt32.TryParse
  let int64 = parseUsing Int64.TryParse
  let uint64 = parseUsing UInt64.TryParse
  let uri = parseUsing (fun s -> Uri.TryCreate(s, UriKind.RelativeOrAbsolute))
  let dateTime = parseUsing (fun s -> DateTime.TryParse(s, CultureInfo.InvariantCulture.DateTimeFormat, DateTimeStyles.RoundtripKind))
  let dateTimeOffset = parseUsing (fun s -> DateTimeOffset.TryParse(s, CultureInfo.InvariantCulture.DateTimeFormat, DateTimeStyles.AssumeUniversal))
  let decimal = parseUsing (fun s -> Decimal.TryParse(s, NumberStyles.Any, CultureInfo.InvariantCulture))
