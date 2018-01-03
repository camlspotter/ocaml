type 'a string_of =
  ('a -> string, [%imp substr "string_of_" (related : 'a)]) Leopard.Implicits.t

val %imp string_of : ?d:'a string_of -> 'a -> string

module X = struct
  type t = Foo
  let string_of_t Foo = "Foo"
end

let () =
  assert (string_of 1.2 = "1.2");
  assert (string_of 1 = "1");
  assert (string_of X.Foo = "Foo")

