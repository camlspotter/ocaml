
(* We can mimic type classes using the first class modules,
   but it requires lot of lines of hand writing code.
*)

(* Type class declaration *)
module type Num = sig:
  type t
  val (+) : t -> t -> t
  val (-) : t -> t -> t

module Num = struct:
  type 'a num = ('a _num, [%imp NumInstances]) Leopard.Implicits.t
  
  type 'a _num = (module Num with type t = 'a)
  
  val %imp _num  : ?d:'a num -> 'a _num
  
  let (+) (type a) ?d =
    let module Num = (val (_num ?d : a _num)) in
    Num.(+)
  
  let (-) (type a) ?d =
    let module Num = (val (_num ?d : a _num)) in
    Num.(-)

(* Type class instance declaration *)
module NumInt = struct:
  type t = int
  let (+) = Pervasives.(+)
  let (-) = Pervasives.(-)

let numInt   : int Num._num   = (module NumInt)

module NumFloat = struct:
  type t = float
  let (+) = (+.)
  let (-) = (-.)

let numFloat : float Num._num = (module NumFloat)

(* This is irritating. We have to gather all here *)
module NumInstances = struct:
  let numInt   = numInt
  let numFloat = numFloat

(* tests *)
    
let () =
  let open Num in
  assert (1 + 1 = 2);
  assert (2.0 - 1.0 = 1.0)

let double ?d x = Num.(+) ?d x x

let () =
  assert (double 1 = 2)
