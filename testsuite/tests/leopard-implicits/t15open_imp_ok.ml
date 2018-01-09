type 'a add = ('a -> 'a -> 'a, [%imp open_imp]) Leopard.Implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

module M = struct:
  let int = (+)
  let float = (+.)

let () = let open %imp Add in assert (add 1 2 = 3)

open %imp Add
let () = assert (add 1.2 3.4 = 4.6)
