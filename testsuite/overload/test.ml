module Num = struct
  module Int = struct
    let (+) = Pervasives.(+)
  end
  
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end

module String = struct
  include String
  let (+) = Pervasives.(^)
end

let (+) (c1 : char) c2 = max c1 c2

module Loaded = struct
  module Num = Num
  module String = String
  external (+) : 'a -> 'a -> 'a = "%OVERLOADED"
  module List = struct
    (* This is defined later than the OVERLOADED (+) but it is ok *)
    let (+) = (@)
  end
end

module Test = struct
  open Loaded
  let () = 
    assert (1 + 2 = 3);
    assert (1.2 + 3.4 = 4.6);
    assert ("hello" + "world" = "helloworld");
    assert ([] + [] = []);
    prerr_endline "OK!"
end

let _ = (Loaded.(+) : int -> int -> int)

(* The following fails since val (+) : char -> char -> char is defined but outside of Loaded *)
(* let _ = (Loaded.(+) : char -> char -> char) *)


module Zero = struct
  external zero : 'a = "%OVERLOADED"
  module Int = struct
    let zero = 0
  end
  module Float = struct
    let zero = 0.0
  end
end

open Zero
let () = assert (Pervasives.(+) zero (int_of_float zero) = zero)
