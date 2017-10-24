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

  module OrigString = String

  module Leopard = struct:
    module DotBracket = struct:
      module String = struct:
        val %overload get : 'a -> int -> 'b
        val %overload set : 'a -> int -> 'b -> unit
        val %overload unsafe_get : 'a -> int -> 'b
        val %overload unsafe_set : 'a -> int -> 'b -> unit
    
        module String = struct:
          let get = OrigString.get
          let set = OrigString.set
          let unsafe_get = OrigString.unsafe_get
          let unsafe_set = OrigString.unsafe_set
    
        module Array = struct:
          let get = Array.get
          let set = Array.set
          let unsafe_get = Array.unsafe_get
          let unsafe_set = Array.unsafe_set

    

  
