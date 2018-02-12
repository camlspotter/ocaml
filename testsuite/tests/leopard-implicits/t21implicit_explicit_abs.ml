(* simple overloading *)
module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : ?_d:'a add -> 'a -> 'a -> 'a

let add' ~_d:(_:'a add) (x : 'a) y = add x y

let add'' ~_d:(_:'a add) (x : 'a) y = add' x y

