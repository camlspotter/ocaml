module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

(*
module M : sig
  val double : ?d: 'a add -> 'a -> 'a
end = struct
  let double ?d x = add ?d x x
end

open M

let () =
  assert (double 1 = 2);
  assert (double 1.2 = 2.4)
*)
  
(* Somehow the type level of double2 is changed *)
let double2 ?d x = add ?d x x
    
