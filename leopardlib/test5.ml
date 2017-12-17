type 'a add = ('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

let double ?(d : 'a add option) x = add x x (* Type constraint is required to state that this is the imp arg *)
