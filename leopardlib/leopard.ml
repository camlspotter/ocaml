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
    let (+)   = Pervasives.(+)
    let (-)   = Pervasives.(-)
    let ( * ) = Pervasives.( * )
    let (/)   = Pervasives.(/)
  
  module Float = struct:
    let (+)   = Pervasives.(+.)
    let (-)   = Pervasives.(-.)
    let ( * ) = Pervasives.( *. )
    let (/)   = Pervasives.(/.)

  module OrigString = String

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

  val %overload __string_get : 'a -> int -> 'b
  val %overload __string_set : 'a -> int -> 'b -> unit
  val %overload __string_unsafe_get : 'a -> int -> 'b
  val %overload __string_unsafe_set : 'a -> int -> 'b -> unit

module Implicits = struct:
  type 'a alias = 'a
              
  type ('a, 'spec) t = 'a

  exception Not_resolved

  let from_Some = function
    | Some x -> x
    | None -> raise Not_resolved
        
  external get : _d:('a, 'spec) t -> 'a = "%identity"
  
  let imp ?_d = from_Some _d
  
  external embed : 'a -> ('a, 'spec) t = "%identity"

  (** [[`M] module_] is the encoding of escaped module name "M" *)
  type 'a module_

  type 'a opened

  type 'a just

  (** to traverse sub-modules  *)
  type 'a deep

  type 'a related

  (** [[`xxx] string] is the encoding of escaped string "xxx" *)
  type 'a string 

  type 'a substr

  type ('a, 'b) filter

  type 'a aggressive

  type open_imp
