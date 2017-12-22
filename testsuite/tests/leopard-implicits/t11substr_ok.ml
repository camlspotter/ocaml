type 'a string_of =
  ('a -> string, [%imp substr "string_of_" Pervasives]) Leopard.Implicits.t

val %imp string_of : ?d:'a string_of -> 'a -> string

let () =
  assert (string_of 1.2 = "1.2");
  assert (string_of 1 = "1")
