(* simple overloading *)
let add ~_d:(_d:('a, [%imp Add]) Leopard.Implicits.t) = Leopard.Implicits.get ~_d

(* =>
let add ~_d:((_d : ('a, [ `Add ]) Leopard.Implicits.t) as __imp__arg__1)  =
  Leopard.Implicits.get ~_d
*)
