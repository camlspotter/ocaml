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

  module Lib = struct
  end
    
