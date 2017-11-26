module Add = struct
  val %overload (+) : 'a -> 'b -> 'c
  module IntIntInt = struct
    let (+) = Pervasives.(+)
  end
  module IntIntFloat = struct
    let (+) x y = float @@ Pervasives.(+) x y
  end
  module FloatFloatFloat = struct
    let (+) = Pervasives.(+.)
  end
end

(* This must be rejected since the paramter x is polymorphic. 
   Since it was not rejected, f was falsely typed as a polymorphic function
   when -no-retype is given:

   val f : 'a -> 'a -> int
*)
let f x = Add.(+) x x + 2

let () = Printf.printf "%d" (f true)
