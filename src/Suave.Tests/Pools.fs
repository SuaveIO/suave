module Pools

open Suave
open Suave.Sockets
open Expecto

type Foo() =
  let foo = "foo"

[<Tests>]
let poolTests (config: SuaveConfig) =
  testList "test pool autogrow capability" [

    testCase "ConcurrentPool" <| fun _ ->
      let pool = ConcurrentPool<Foo>()
      pool.ObjectGenerator <- fun _ -> new Foo()
      for i = 0 to 10 do pool.Push (new Foo())
      for i = 0 to 20 do pool.Pop() |> ignore
      Expect.equal true true "test ran to completion"

  ]