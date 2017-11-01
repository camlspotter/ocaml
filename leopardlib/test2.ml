module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t

let add : ?d:'a add -> 'a -> 'a -> 'a = Leopard.Implicits.imp

let () = assert (add 1 2 = 3)
let () = assert (add 1.2 3.4 = 4.6)
