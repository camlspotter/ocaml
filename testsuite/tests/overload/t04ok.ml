open Lib.Overload

module Lib = struct
end

let () =
  assert (1 + 2 = 3); (* Lib.Overload.Int *)
  assert (1.2 + 3.4 = 4.6)
    
