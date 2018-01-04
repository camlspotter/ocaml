module Show = struct
  let string_of_int = string_of_int
  let string_of_float = string_of_float
  let string_of_list f xs = "[" ^ String.concat "; " (List.map f xs) ^ "]"
end

type 'a string_of =
  ('a -> string, [%imp aggressive (substr "string_of_" Show)]) Leopard.Implicits.t

val %imp string_of : ?d:'a string_of -> 'a -> string

let () =
  assert (string_of 1.2 = "1.2");
  assert (string_of 1 = "1");
  assert (string_of [1] = "[1]");
  assert (string_of [[1;2]] = "[[1; 2]]");
