type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : ?_d:'a add -> 'a -> 'a -> 'a

let double ?(d : 'a add option) (x : 'a) = add x x
