module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

let double ?d x = add ?d x x

let () =
  assert (double 1 = 2);
  assert (double 1.2 = 2.4)

