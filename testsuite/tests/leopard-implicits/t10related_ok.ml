type 'a string_of = ('a -> string, [%imp (related : 'a)]) Leopard.Implicits.t

val %imp string_of : ?_d:'a string_of -> 'a -> string

let () =
  (* Note: the search space is huge: all the values defined in Pervasives *)
  assert (string_of 1.2 = "1.2")


