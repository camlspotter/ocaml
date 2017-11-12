module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

(* If we perform unifications in implicit.ml without thinking, 
   somehow the type level of double is changed *)
let double ?d x = add ?d x x
    
