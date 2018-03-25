(* simple overloading *)
let add ~_d:(_d:('a, 'b) Leopard.Implicits.t) = Leopard.Implicits.get ~_d

(* =>
Error: This type 'b is where a type of encoded string was expected 
({id=854;level=100000000;desc= Tvar "b"})

Hmm, this is not very very required...
*)
