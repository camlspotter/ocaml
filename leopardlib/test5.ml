type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

(* Type constraint is required to state that [d] is the imp arg related with [x] *)
let double : ?d:'a add -> 'a -> 'a = fun ?d x -> add x x 
