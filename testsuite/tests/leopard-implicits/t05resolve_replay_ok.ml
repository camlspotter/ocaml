type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : _d:'a add -> 'a -> 'a -> 'a

module Add = struct:
  let int = (+)

let r = ref [] (* The variable should be unified with int *)

let f = try add (List.hd !r) (List.hd !r) with Failure "hd" -> 0

