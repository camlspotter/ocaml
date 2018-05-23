module Add = struct
  let int = (+)
  let float = (+.)
end

let () = 
  assert ([%imp Add] 1 2 = 3);
  assert ([%imp Add] 1.2 3.4 = 4.6)

let add = [%imp Add]

let () =
  assert (add 1 2 = 3);
  assert (add 1.2 3.4 = 4.6)

let double ?_d x = add ?_d x x

let () =
  assert (double 1 = 2);
  assert (double 1.2 = 2.4)
