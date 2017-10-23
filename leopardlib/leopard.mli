val __array_get : 'a array -> int -> 'a
val __array_set : 'a array -> int -> 'a -> unit
val __array_unsafe_get : 'a array -> int -> 'a
val __array_unsafe_set : 'a array -> int -> 'a -> unit

val __string_get : string -> int -> char
val __string_set : bytes -> int -> char -> unit
val __string_unsafe_get : string -> int -> char
val __string_unsafe_set : bytes -> int -> char -> unit

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

  val %overload __string_get : 'a -> int -> 'b
  val %overload __string_set : 'a -> int -> 'b -> unit
  val %overload __string_unsafe_get : 'a -> int -> 'b
  val %overload __string_unsafe_set : 'a -> int -> 'b -> unit
    
  module String : sig:
    val __string_get : string -> int -> char
    val __string_set : bytes -> int -> char -> unit
    val __string_unsafe_get : string -> int -> char
    val __string_unsafe_set : bytes -> int -> char -> unit
  
  module Array : sig:
    val __string_get : 'a array -> int -> 'a
    val __string_set : 'a array -> int -> 'a -> unit
    val __string_unsafe_get : 'a array -> int -> 'a
    val __string_unsafe_set : 'a array -> int -> 'a -> unit
      
