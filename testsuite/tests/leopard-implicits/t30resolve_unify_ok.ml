type 'a add = ('a -> 'a -> 'a, [%imp just Add]) Leopard.Implicits.t

val %imp add : _d:'a add -> 'a -> 'a -> 'a

module Add = struct:
  let int = (+)

let r = ref []

let f = add (List.hd !r) (List.hd !r)

(* The resolver chooses `Add.int` for `add` since there is only one candidate.

   BUG (already fixed): Apart from it is ok or not, the resolution result 
     was never reflected to the type of `r`. 
*)
