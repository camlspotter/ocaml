class c = object
  method m = 1
end

(*
class c =
  Fatal error: exception Assert_failure("parsing/pprintast.ml", 629, 8)
Raised at file "parsing/pprintast.ml", line 629, characters 8-20
*)

class c1 = object
  val v = 1
end

class c2 = object
  val v = 1
  method m1 = v
  method m2 = v
end

