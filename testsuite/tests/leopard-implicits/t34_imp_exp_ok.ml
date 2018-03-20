(* simple overloading *)
module Add = struct
  let int = (+)
  let float = (+.)
end

let () = assert ([%imp Add] 1 2 = 3)
let () = assert ([%imp Add] 1.2 3.4 = 4.6)

(*
let add : _d:('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t -> 'a -> 'a -> 'a = fun ~_d -> [%imp Add]

let () = assert (add 1 2 = 3)
  
let double ~_d x = add ~_d x x

let () = assert (double 1 = 2)
*)
  
(* 
   => let add ~d:(x : ('a -> 'a -> 'a, [ `Add ]) t) = (get (embed (get x)) : 'a -> 'a -> 'a) 

   == let add ~d:(x : ('a -> 'a -> 'a, [ `Add ]) Leopard.Implicits.t) = (x : 'a -> 'a -> 'a)
   
   This should be a bit inefficient compared with
   
   => val %imp add : _d: 'a -> 'a -> 'a 
*)

(* 
let add ~_d:(_ : ('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t) = [%imp Add]

does not work, since the type of the RHS is ('b, [%imp Add])
*)

(* 
let add = [%imp Add]

does not work since we have no auto-abs for now
*)
