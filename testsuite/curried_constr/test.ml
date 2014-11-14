let x : int option = Some 1
let x = (Some) 1
let x = Some @@ 1
(* let x = (Some..) 1    Error: Unary constructor cannot be curried. *)
let x = (None)
(* let x = (None..)    Error: Nullary constructor cannot be curried. *)

type t = Foo of int * float
let x : t = (Foo) (1,1.0)

let x : t = (Foo..) 1 1.0
let x : int -> float -> t = !Foo (* equivalent with (Foo..) *)
let x : t = (Foo..) 1 1.0
let x : float -> t = (Foo..) 1
let x : (int * float) -> t = Foo
let x : (int * float) -> t = fun x -> (Foo) x
