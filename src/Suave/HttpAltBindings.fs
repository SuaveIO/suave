namespace Suave

module HttpAltBindings =
  open Suave.Types


  //Reexpose the non-overriden modules from Http.fs
  let inline suceed x = Http.succeed x
  let fail = Http.fail
  let never = Http.never
  let choose = Http.choose 
  let inline warbler f a = Http.warbler f a
  let inline cnst x = Http.cnst x
  let cond = Http.cond


  //Here we expose Suave's WebPart composition with the more standard >=> operator
  //Which frees other libraries to use >>= in the more tranditional way.


  //Using a dummy type here so that our operator definition doesn't automatically smash
  //anyone who might have defined these operators on other types
  //technique inspired by
  //http://stackoverflow.com/questions/19682432/global-operator-overloading-in-f
  //certainly not pure idomatic F# but it gives downstream library implementers the options 
  type BindIt = BindIt with
    static member inline (>=>) (BindIt,first) = fun second -> Http.bind second first
    static member inline (<=<) (BindIt,first) = fun second -> Http.bind second first

    //These are the fall through cases for other definitions of >=>
    static member inline (>=>) (BindIt,first) = fun second -> first >=> second
    static member inline (<=<) (BindIt,first) = fun second -> first <=< second


  let inline (>=>) first second = (BindIt, first) >=> second
  let inline (<=<) first second = (BindIt, first) <=< second



  module Response = Http.Response
  module Intermediate = Http.Intermediate
  module Successful = Http.Successful
  module Redirection = Http.Redirection
  module RequestErrors = Http.RequestErrors
  module ServerErrors = Http.ServerErrors
  module Applicatives = Http.Applicatives
  module Files = Http.Files
  module Authentication = Http.Authentication
