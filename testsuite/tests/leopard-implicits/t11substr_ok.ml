type 'a string_of =
  ('a -> string, [%imp filter (substr "string_of_") (just Pervasives)]) Leopard.Implicits.t

val %imp string_of : ?d:'a string_of -> 'a -> string

let () =
  assert (string_of 1.2 = "1.2");
  assert (string_of 1 = "1")
