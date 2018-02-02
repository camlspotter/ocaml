type 'a add = ('a -> 'a -> 'a, [%imp deep (just Add)]) Leopard.Implicits.t

val %imp add : ?_d:'a add -> 'a -> 'a -> 'a

module M1 = struct:
  module Add = struct:
    let int = (+)
    let float = (+.)
  
  let () =
    assert (add 1 2 = 3);
    assert (add 1.2 3.4 = 4.6)

module M2 = struct:
  module Add = struct:
    let string = (^)

  let () =
    assert (add "a" "b" = "ab")

module M3 = struct:
  open M1 (* Use M1.Add for add *)
  let () =
    assert (add 1 2 = 3)

module M4 = struct:
  (* Instances are in sub modules *)
  module Add = struct:
    module M1 = M1.Add
    module M2 = M2.Add

  let () =
    assert (add "a" "b" = "ab");
    assert (add 1 2 = 3);
    assert (add 1.2 3.4 = 4.6)
