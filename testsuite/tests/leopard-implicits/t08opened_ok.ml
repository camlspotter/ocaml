type 'a add = ('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t

val %imp add : ?_d:'a add -> 'a -> 'a -> 'a

module T1 = struct:
  module Add = struct:
    let int = (+)
    let float = (+.)

  let () =
    assert (add 1 2 = 3);
    assert (add 1.2 3.4 = 4.6)

module T2 = struct:
  module M = struct:
    module Add = struct:
      let int = (+)
      let float = (+.)

  open M

  let () =
    assert (add 1 2 = 3);
    assert (add 1.2 3.4 = 4.6)


