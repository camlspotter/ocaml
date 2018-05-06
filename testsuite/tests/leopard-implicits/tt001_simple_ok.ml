module Add = struct
  let int = (+)
  let float = (+.)
end

let () = assert ([%imp Add] 1 2 = 3)  
let () = assert ((Leopard.Implicits.get : _d:(_, [%imp Add]) Leopard.Implicits.t -> _) 1 2 = 3)
