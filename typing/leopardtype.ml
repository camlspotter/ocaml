let curried_constr = ref true
let overload = ref true

let enable_leopard () =
  curried_constr := true;
  overload := true
    
let disable_leopard () =
  curried_constr := false;
  overload := false
    
let without_leopard f =
  disable_leopard ();
  match f () with
  | exception e ->
      enable_leopard ();
      raise e
  | res ->
      enable_leopard ();
      res

let is_leopardlib_available _env =
  try
    (* ignore (Env.lookup_module ~load:true Longident.(Lident "Leopard") env); true *)
    true
  with
  | _ -> false

let init env = Leopardppx.with_leopardlib := is_leopardlib_available env 

