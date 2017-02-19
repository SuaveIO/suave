module Suave.Tests.ExpectoExtensions

open Expecto
open Expecto.Tests
open Expecto.Impl
open System
open System.Linq
open System.Reflection

type MemberInfo with
  member m.HasAttributePred (pred: Type -> bool) =
    m.GetCustomAttributes true
    |> Seq.filter (fun a -> pred(a.GetType()))
    |> Seq.length |> (<) 0

  member m.HasAttributeType (attr: Type) =
    m.HasAttributePred ((=) attr)

let testFromMemberWithParam (param : 't) (m: MemberInfo): Test option =
  [m]
  |> List.filter (fun m -> m.HasAttributeType typeof<TestsAttribute>)
  |> List.choose (fun m ->
    match box m with
    | :? FieldInfo as m ->
      if m.FieldType = typeof<Test>
        then Some(unbox (m.GetValue(null)))
        else None
    | :? MethodInfo as m ->
      if m.ReturnType = typeof<Test>
        then Some(unbox (m.Invoke(null, [| param :> obj |])))
        else None
    | :? PropertyInfo as m ->
      if m.PropertyType = typeof<Test>
        then Some(unbox (m.GetValue(null, null)))
        else None
    | _ -> None)
  |> List.tryFind (fun _ -> true)

let testFromTypeWithParam param =
  let asMembers x = Seq.map (fun m -> m :> MemberInfo) x
  let bindingFlags = BindingFlags.Public ||| BindingFlags.Static
  fun (t: Type) ->
    [ t.GetMethods bindingFlags |> asMembers
      t.GetProperties bindingFlags |> asMembers
      t.GetFields bindingFlags |> asMembers ]
    |> Seq.collect id
    |> Seq.choose (testFromMemberWithParam param)
    |> Seq.toList
    |> listToTestListOption

/// Scan filtered tests marked with TestsAttribute from an assembly
let testFromAssemblyWithFilterAndParam typeFilter (a: Assembly) param =
  a.GetExportedTypes()
  |> Seq.filter typeFilter
  |> Seq.choose (testFromTypeWithParam param)
  |> Seq.toList
  |> listToTestListOption

/// Scan tests marked with TestsAttribute from an assembly
let testFromAssemblyWithParam asm param =
  testFromAssemblyWithFilterAndParam (fun _ -> true) asm param

let defaultMainThisAssemblyWithParam param args =

  let tests =
      match testFromAssemblyWithParam (Assembly.GetEntryAssembly()) param with
      | Some t -> t
      | None -> failwith "Found no tests."

  match ExpectoConfig.fillFromArgs defaultConfig args with
  | ArgsRun cfg -> runTests cfg tests
  | _ -> 1