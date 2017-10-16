val without_leopard : (unit -> 'a) -> 'a
(** [without_leopard f] runs [f] disabling all the features of leopard *)

val overload : bool ref
