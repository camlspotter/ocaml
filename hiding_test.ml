module X = struct
  let a = 1
  let b = 2
end

module Y = struct
  let a = 3
  let c = 4
end

open {X - { a }}
open Y

let () = Printf.printf "%d" (b + a + c)

 
