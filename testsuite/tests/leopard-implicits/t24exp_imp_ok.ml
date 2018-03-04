(* simple overloading *)
module Add = struct
  let int = (+)
  let float = (+.)
end

external get : ('a, 'spec) Leopard.Implicits.t -> 'a = "%identity"
  
let () = assert (get [%imp just Add] 1 2 = 3)
