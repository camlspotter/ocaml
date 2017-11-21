val %overload (+)   : 'a -> 'a -> 'a

module Int = struct
  let (+) = Pervasives.(+)
end
module Float = struct
  let (+) = Pervasives.(+.)
end

let _ = 
  assert (1 + 2 = 3);
  assert (1.2 + 3.4 = 4.6); (* See it is not +. but + !!! *)
  prerr_endline "OK!"

