let overload = ref true

let enable_leopard () =
  overload := true
    
let disable_leopard () =
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
