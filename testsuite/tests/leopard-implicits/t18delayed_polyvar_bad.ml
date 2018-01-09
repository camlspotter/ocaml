(* Keigoi pointed me this issue *)

type 'a read = (string -> 'a, [%imp Read]) Leopard.Implicits.t

val %imp read : ?_d:'a read -> string -> 'a
  
module Read = struct
  let _a : string -> [`A] = function "`A" -> `A | s -> failwith "read error"
end

let read_poly : string -> 'a = fun _ -> assert false

let () = 
  read_poly "`A" |> function
  | `A -> print_endline "A"
  | `B -> print_endline "B"

let () = 
  read "`A" |> function
  | `A -> print_endline "A"
  | `B -> print_endline "B"
            
(* The above code is resolved to the following, which is ill-typed.

let () = 
  Read._a |> function
  | `A -> print_endline "A"
  | `B -> print_endline "B"

This is due to the delayed test around polymorphic variants.
*)

(* What I can do?
   
   Not so much, since, the delayed checks run after the resolution
*)
