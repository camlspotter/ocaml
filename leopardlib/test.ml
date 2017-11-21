open Printf
open Leopard.Overload

let () =
  assert ((+) 1 2 = 3);
  assert (1.2 + 3.4 = 4.6);
  Printf.printf "%c\n" ("hello".[0]);
  Printf.printf "%d\n" ([|1;2;3|].[0]);
  ()
