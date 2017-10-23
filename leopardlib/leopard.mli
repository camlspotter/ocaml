module Overload : sig:
  external ( + ) : 'a -> 'a -> 'a = "%OVERLOADED"
  external ( - ) : 'a -> 'a -> 'a = "%OVERLOADED"
  external ( * ) : 'a -> 'a -> 'a = "%OVERLOADED"
  external ( / ) : 'a -> 'a -> 'a = "%OVERLOADED"

  module Int : sig:
    val ( + ) : int -> int -> int
    val ( - ) : int -> int -> int
    val ( * ) : int -> int -> int
    val ( / ) : int -> int -> int
  
  module Float : sig:
    val ( + ) : float -> float -> float
    val ( - ) : float -> float -> float
    val ( * ) : float -> float -> float
    val ( / ) : float -> float -> float


  
