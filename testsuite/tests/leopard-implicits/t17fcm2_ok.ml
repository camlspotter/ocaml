(* We can mimic type classes using the first class modules,
   but it requires lot of lines of hand writing code.
*)

(* Type class declaration *)
module type Num = sig:
  type t
  val (+) : t -> t -> t
  val (-) : t -> t -> t

module Num = struct:
  type 'a num = ((module Num with type t = 'a), [%imp modulex NumInstances]) Leopard.Implicits.t
  
  val %imp _num  : ?d:'a num -> (module Num with type t = 'a)
  
  let (+) (type a) ?d =
    let module Num = (val (_num ?d : (module Num with type t = a))) in
    Num.(+)
  
  let (-) (type a) ?d =
    let module Num = (val (_num ?d : (module Num with type t = a))) in
    Num.(-)

(* Type class instance declaration *)
module NumInt = struct:
  type t = int
  let (+) = Pervasives.(+)
  let (-) = Pervasives.(-)

let numInt   : (module Num with type t = int) = (module NumInt)

module NumFloat = struct:
  type t = float
  let (+) = (+.)
  let (-) = (-.)

let numFloat : (module Num with type t = float) = (module NumFloat)

(* This is irritating. We have to gather all here *)
module NumInstances = struct:
  module I = NumInt
  module F = NumFloat

(* tests *)
    
let () =
  let open Num in
  assert (1 + 1 = 2);
  assert (2.0 - 1.0 = 1.0)

let double ?d x = Num.(+) ?d x x

let () =
  assert (double 1 = 2)
