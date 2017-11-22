module Loaded = struct
  val %overload (+) : 'a -> 'a -> 'a
  module Int = struct
    let (+) = Pervasives.(+)
  end
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end

let _ = 
  assert (Loaded.(+) 1 2 = 3);
  assert (Loaded.(+) 1.2 3.4 = 4.6); (* See it is not +. but + !!! *)
  prerr_endline "OK!"

module Loaded2 = struct
  val %overload (+) : 'a -> 'a -> 'a
  module A = struct
    module Int = struct
      let (+) = Pervasives.(+)
    end
  end
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end

let _ = 
  assert (Loaded2.(+) 1 2 = 3);
  assert (Loaded2.(+) 1.2 3.4 = 4.6); (* See it is not +. but + !!! *)
  prerr_endline "OK!"

module Loaded3 = struct
  include Loaded
  module String = struct
    let (+) = Pervasives.(^)
  end
end

let _ = 
  assert (Loaded3.(+) 1 2 = 3);
  assert (Loaded3.(+) 1.2 3.4 = 4.6);
  assert (Loaded3.(+) "a" "b" = "ab");
  prerr_endline "OK!"


