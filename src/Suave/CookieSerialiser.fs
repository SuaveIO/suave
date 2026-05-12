namespace Suave

open System.Diagnostics.CodeAnalysis

type CookieSerialiser =
  abstract serialise : Map<string, obj> -> byte []
  abstract deserialise : byte [] -> Map<string, obj>

open System.Text.Json

/// Default JSON-based cookie serializer.
///
/// NOTE: this implementation serializes `Map<string,obj>` polymorphically using
/// the reflection-based `System.Text.Json` APIs. It is therefore NOT compatible
/// with .NET Native AOT or aggressive trimming. Consumers targeting AOT must
/// implement `CookieSerialiser` themselves over a closed payload schema (e.g.
/// using a `JsonSerializerContext` source generator) and assign it to
/// `SuaveConfig.cookieSerialiser`.
///
/// The historical name `BinaryFormatterSerialiser` is preserved for source
/// compatibility; the underlying engine has been `System.Text.Json` for a while.
[<RequiresDynamicCode("BinaryFormatterSerialiser uses reflection-based System.Text.Json over Map<string,obj>; not compatible with Native AOT. Provide a custom CookieSerialiser for AOT scenarios.")>]
[<RequiresUnreferencedCode("BinaryFormatterSerialiser uses reflection-based System.Text.Json over Map<string,obj>; not compatible with trimming. Provide a custom CookieSerialiser.")>]
type BinaryFormatterSerialiser() =
  interface CookieSerialiser with
    member x.serialise m =
      JsonSerializer.SerializeToUtf8Bytes<_>(m)

    member x.deserialise data =
      JsonSerializer.Deserialize<_>(data)
