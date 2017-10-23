[@@@ocaml.warning "-3"]

let __array_get = Array.get
let __array_set = Array.set
let __array_unsafe_get = Array.unsafe_get
let __array_unsafe_set = Array.unsafe_set
let __string_get = String.get
let __string_set = String.set 
let __string_unsafe_get = String.unsafe_get
let __string_unsafe_set = String.unsafe_set

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

  val %overload __string_get : 'a -> int -> 'b
  val %overload __string_set : 'a -> int -> 'b -> unit
  val %overload __string_unsafe_get : 'a -> int -> 'b
  val %overload __string_unsafe_set : 'a -> int -> 'b -> unit

    
  module String = struct:
    let __string_get = OrigString.get
    let __string_set = OrigString.set
    let __string_unsafe_get = OrigString.unsafe_get
    let __string_unsafe_set = OrigString.unsafe_set

  module Array = struct:
    let __string_get = Array.get
    let __string_set = Array.set
    let __string_unsafe_get = Array.unsafe_get
    let __string_unsafe_set = Array.unsafe_set

  
