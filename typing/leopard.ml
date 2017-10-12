let enable_leopard () =
  ()
    
let disable_leopard () =
  ()
    
let without_leopard f =
  disable_leopard ();
  match f () with
  | exception e ->
      enable_leopard ();
      raise e
  | res ->
      enable_leopard ();
      res
