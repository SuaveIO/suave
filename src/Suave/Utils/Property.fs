namespace Suave.Utils
open System

type internal Property<'T,'P> = Suave.Utils.Aether.Lens<'T, 'P>

[<AutoOpen>]
module internal PropertyModule =
  let internal Property<'T,'P> getter setter : Property<'T,'P> = (getter, setter)