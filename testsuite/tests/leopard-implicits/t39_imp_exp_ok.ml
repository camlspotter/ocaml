(* simple overloading *)
let add ~_d:(_ : ('a, [%imp Add]) Leopard.Implicits.t) = [%imp Add]
