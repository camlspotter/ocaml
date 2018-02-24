type 'a add_inst = AddInst of ('a -> 'a -> 'a)

type 'a add = ('a add_inst, [%imp Add]) Leopard.Implicits.t

val %imp add' : ?_d:'a add -> 'a add_inst

let add ?_d = let AddInst f = add' ?_d in f

module Add = struct:
  let int = AddInst (+)
  let float = AddInst (+.)

let () =
  assert (add 1 2 = 3);
  assert (add 1.2 3.4 = 4.6)
