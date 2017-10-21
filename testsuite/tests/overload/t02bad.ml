module Loaded = struct
  val %overload (+) : 'a -> 'a -> 'a
  module Int = struct
    let (+) = Pervasives.(+)
  end
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end

open Loaded
let double x = x + x
