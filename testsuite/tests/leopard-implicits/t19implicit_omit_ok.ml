(* simple overloading *)
module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

let add ~_d x y = add ~d:_d x y

let () = assert (add 1 2 = 3)

let double x = add x x
  
(*
  let () = 
  let imp = (+) in
  assert (add 1 2 = 3)

let double x = add x x

let () = 
  let imp = (+) in
  assert (double 2 = 4)


*)
  
