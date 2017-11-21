module Overload = struct:

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

  val %overload (+)   : 'a -> 'a -> 'a
  val %overload (-)   : 'a -> 'a -> 'a
  val %overload (/)   : 'a -> 'a -> 'a
  val %overload ( * ) : 'a -> 'a -> 'a
  
  module Lib = struct
  end
    
