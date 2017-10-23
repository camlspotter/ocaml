val without_leopard : (unit -> 'a) -> 'a
(** [without_leopard f] runs [f] disabling all the features of leopard *)

val curried_constr : bool ref
val overload : bool ref

val is_leopardlib_available : Env.t -> bool

val init : Env.t -> unit
