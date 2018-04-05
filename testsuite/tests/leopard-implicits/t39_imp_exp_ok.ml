(* simple overloading *)
let add ~_d:(_ : ('a, [%imp Add]) Leopard.Implicits.t) = [%imp Add]

(* buggy
val add : _d:('a, [ `Add ]) Leopard.Implicits.t -> 'imp__any_var1
*)
