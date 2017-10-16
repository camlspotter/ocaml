module Loaded = struct
  external (+) : 'a -> 'a -> 'a = "%OVERLOADED"
  module Int = struct
    let (+) = Pervasives.(+)
  end
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end

open Loaded
let double x = x + x
