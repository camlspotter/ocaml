(* simple overloading *)
module Add = struct
  let int = (+)
  let float = (+.)
end

type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : ?_d:'a add -> 'a -> 'a -> 'a

let add ~_d x y = add ~_d x y

(* we have no auto-abs

let double x = add x x
    
let rec times x = function
  | 1 -> x
  | n -> add x (times x (n-1))

let () =
  assert (times 3 1 = 3);
  assert (times 3 2 = 6);
  assert (times 3 3 = 9);
  assert (times 2.0 3 = 6.0)

let rec  times' x n = times x n
and times x = function
  | 1 -> x
  | n -> add x (times' x (n-1))
  
let () =
  assert (times 2 3 = 6);
  assert (times 2.0 3 = 6.0)
*)
                
