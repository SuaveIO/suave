﻿module Suave.Tests.FuchuExtensions

open Fuchu
open Fuchu.Helpers
open Fuchu.Tests
open Fuchu.Impl

open System
open System.Linq
open System.Reflection

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
                              then 
                                Some(unbox (m.Invoke(null, [|param :> obj |])))
                              else None
                      | :? PropertyInfo as m -> 
                          if m.PropertyType = typeof<Test>
                              then Some(unbox (m.GetValue(null, null)))
                              else None
                      | _ -> None)
  |> List.tryFind (fun _ -> true)

let listToTestListOption = 
  function
  | [] -> None
  | x -> Some (TestList x)

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
let testFromAssemblyWithParam asm param = testFromAssemblyWithFilterAndParam (fun _ -> true) asm param

let defaultMainThisAssemblyWithParam param args = 

  let tests =
      match testFromAssemblyWithParam (Assembly.GetEntryAssembly()) param with
      | Some t -> t
      | None -> TestList []

  defaultMain tests args
