open Leopard

module String = struct:
  external get : 'a -> int -> 'b = "%OVERLOADED" 
  module String = struct:
    let get = String.get
  module Array = struct:
    let get = Array.get
  
let () =
  assert (1 + 2 = 3);
  assert (1.2 + 3.4 = 4.6);
  Printf.printf "%c\n" ("hello".[0]);
  Printf.printf "%d\n" ([|1;2;3|].[0]);
    
    
