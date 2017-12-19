type 'a add = ('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

module Add = struct:
  let int = (+)

let r = ref [] (* The variable should be unified with int *)

let f = add (List.hd !r) (List.hd !r)
