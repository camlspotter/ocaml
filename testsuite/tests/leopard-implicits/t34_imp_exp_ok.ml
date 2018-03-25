(* simple overloading *)
module Add = struct
  let int = (+)
  let float = (+.)
end

let () = assert ([%imp Add] 1 2 = 3)
let () = assert ([%imp Add] 1.2 3.4 = 4.6)
