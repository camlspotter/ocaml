module DotBracket : sig:
  module Array : sig:
    val get : 'a array -> int -> 'a
    val set : 'a array -> int -> 'a -> unit
    val unsafe_get : 'a array -> int -> 'a
    val unsafe_set : 'a array -> int -> 'a -> unit
  module String : sig:
    val get : string -> int -> char
    val set : bytes -> int -> char -> unit
    val unsafe_get : string -> int -> char
    val unsafe_set : bytes -> int -> char -> unit

module Overload : sig:
  val %overload ( + ) : 'a -> 'a -> 'a
  val %overload ( - ) : 'a -> 'a -> 'a
  val %overload ( * ) : 'a -> 'a -> 'a
  val %overload ( / ) : 'a -> 'a -> 'a

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

  module Leopard : sig:
    module DotBracket : sig:  
      module String : sig:
        val %overload get : 'a -> int -> 'b
        val %overload set : 'a -> int -> 'b -> unit
        val %overload unsafe_get : 'a -> int -> 'b
        val %overload unsafe_set : 'a -> int -> 'b -> unit
    
        module String : sig:
          val get : string -> int -> char
          val set : bytes -> int -> char -> unit
          val unsafe_get : string -> int -> char
          val unsafe_set : bytes -> int -> char -> unit
    
        module Array : sig:
          val get : 'a array -> int -> 'a
          val set : 'a array -> int -> 'a -> unit
          val unsafe_get : 'a array -> int -> 'a
          val unsafe_set : 'a array -> int -> 'a -> unit

      
