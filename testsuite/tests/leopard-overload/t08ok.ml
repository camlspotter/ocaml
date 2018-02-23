module Overload = struct:

  val %overload (+)   : 'a -> 'a -> 'a
  
  module Int = struct:
    let (+)   = Pervasives.(+)
  
  module Float = struct:
    let (+)   = Pervasives.(+.)

  module OrigString = String

  module String = struct:
    let __string_get = OrigString.get
    let __string_unsafe_get = OrigString.unsafe_get

  module Array = struct:
    let __string_get = Array.get
    let __string_unsafe_get = Array.unsafe_get

  val %overload __string_get : 'a -> int -> 'b
  val %overload __string_set : 'a -> int -> 'b -> unit
  val %overload __string_unsafe_get : 'a -> int -> 'b
  val %overload __string_unsafe_set : 'a -> int -> 'b -> unit

open Overload
    
let () =
  assert ((+) 1 2 = 3);
  assert (1.2 + 3.4 = 4.6);
  assert ("hello".[0] = 'h');
  assert ([|1;2;3|].[0] = 1)
