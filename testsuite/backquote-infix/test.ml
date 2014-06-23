let div = (/)

let () = assert (24 ``div 6 = 4) (* works as a binop *)
let () = assert (4 + 24 ``div 6 = 8) (* stronger than + *)
let () = assert (24 ``div 6 + 6= 10) (* stronger than + *)
let () = assert (24 ``div 6 ``div 2 = 2) (* left associative *)
let () = assert (24 / 6 ``div 2 = 2) (* left associative *)
let () = assert (24 ``div 6 / 2 = 2) (* left associative *)

