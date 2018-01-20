let curried_constr = ref true
let overload = ref true
let implicits = ref true

let enable_leopard () =
  curried_constr := true;
  overload := true;
  implicits := true
    
let disable_leopard () =
  curried_constr := false;
  overload := false;
  implicits := true
    
let without_leopard f =
  disable_leopard ();
  match f () with
  | exception e ->
      enable_leopard ();
      raise e
  | res ->
      enable_leopard ();
      res

