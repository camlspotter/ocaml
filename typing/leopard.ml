let curried_constr = ref true

let enable_leopard () =
  curried_constr := true
    
let disable_leopard () =
  curried_constr := false
    
let without_leopard f =
  disable_leopard ();
  match f () with
  | exception e ->
      enable_leopard ();
      raise e
  | res ->
      enable_leopard ();
      res
