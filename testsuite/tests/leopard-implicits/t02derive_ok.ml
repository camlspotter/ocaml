type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : ?_d:'a add -> 'a -> 'a -> 'a

(* If we perform unifications in implicit.ml without thinking, 
   somehow the type level of double is changed *)
let double ?_d x = add ?_d x x
(*
let double2 ~d x = add ~d x x
let double3 d x = add ?_d x x
*)
