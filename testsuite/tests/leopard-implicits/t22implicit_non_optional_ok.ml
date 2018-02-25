(* simple overloading *)
module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : _d:'a add -> 'a -> 'a -> 'a

let () =
  assert (add 1 2 = 3);
  assert (add 1.2 3.4 = 4.6)

let double ~_d:(_:'a add) (x :'a) = add x x

let () =
  assert (double 2 = 4);
  assert (double 2.3 = 4.6)

let quad ~_d:(_:'a add) (x : 'a) = double (double x)

let () =
  assert (quad 2 = 8)
  
(* This fails...
let double_bad ~_d x = add x x
*)
  
(*
let double ~_d x = add x x (* omitted imp arg must fail the typing *)
 *)
