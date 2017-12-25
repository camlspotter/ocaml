type 'a add = ('a -> 'a -> 'a, [%imp opened (just Add)]) Leopard.Implicits.t

val %imp add : ?d:'a add -> 'a -> 'a -> 'a

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
      module X = struct:
        let int2 x y = 0
 
  open M

  let () =
    assert (add 1 2 = 3);
    assert (add 1.2 3.4 = 4.6)


