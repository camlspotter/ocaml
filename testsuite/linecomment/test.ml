let _ = prerr_endline "1"

/// let _ = assert false

(* hello
   /// this is not considered as nested
*)

(* hello /// not nested *)

let _ = prerr_endline "2"

/// (* hello *)

let _ = prerr_endline "3 /// 4"

///

let _ = prerr_endline "5"///

//// 

(* the following end without newline *)
///