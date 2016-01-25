module Pools

open Suave
open Suave.Sockets
open Fuchu

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
      Assert.Equal("test ran to completion", true, true)

    testCase "BufferManager" <| fun _ ->
      // 5 buffers
      let bufferManager = new BufferManager(2560, 512, config.logger, true)
      for i = 0 to 10 do bufferManager.PopBuffer() |> ignore
      Assert.Equal("test ran to completion", true, true)]