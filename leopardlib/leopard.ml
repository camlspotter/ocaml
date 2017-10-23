module DotBracket = struct:
  module Array = struct:
    let get = Array.get
    let set = Array.set
    let unsafe_get = Array.unsafe_get
    let unsafe_set = Array.unsafe_set

  module String = struct:
    let get = String.get
    let set = String.set
    let unsafe_get = String.unsafe_get
    let unsafe_set = String.unsafe_set

module Overload = struct:

  val %overload (+)   : 'a -> 'a -> 'a
  val %overload (-)   : 'a -> 'a -> 'a
  val %overload (/)   : 'a -> 'a -> 'a
  val %overload ( * ) : 'a -> 'a -> 'a
  
  module Int = struct:
    let (+) = (+)
    let (-) = (-)
    let ( * ) = ( * )
    let (/) = (/)
  
  module Float = struct:
    let (+) = (+.)
    let (-) = (-.)
    let ( * ) = ( *. )
    let (/) = (/.)

  
