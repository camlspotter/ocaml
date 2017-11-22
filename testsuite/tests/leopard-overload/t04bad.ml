(* This checks the shadowing.  If a resolved path is shadowed, 
   the resolution fails.
 *)
open Lib.Overload

module Lib = struct
end

let () =
  assert (1 + 2 = 3);
  assert (1.2 + 3.4 = 4.6)


