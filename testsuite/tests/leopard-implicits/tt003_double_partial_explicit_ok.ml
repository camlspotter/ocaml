open Leopard.Implicits

let double : _d:('a -> 'a -> 'a, [%imp Add]) t -> 'a -> 'a = fun ~_d x ->
  (get : _d:('a -> 'a -> 'a, [%imp Add]) t -> 'a -> 'a -> 'a) x x

module Add = struct
  let int = (+)
  let float = (+.)
end

let () = assert (double 1 = 2)



  
