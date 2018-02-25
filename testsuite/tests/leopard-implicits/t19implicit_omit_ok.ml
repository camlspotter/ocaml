(* simple overloading *)
module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : _d:'a add -> 'a -> 'a -> 'a

let () = assert (add 1 2 = 3)

let add ~_d x y = add ~_d x y
  
let () = assert (add 1 2 = 3)
    
(* We have no auto-abs
let add' x y = add x y

let () = assert (add' 1 2 = 3)

let double x = add x x

let () = assert (double 2 = 4)
 *)
