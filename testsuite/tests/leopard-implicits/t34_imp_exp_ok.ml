(* simple overloading *)
module Add = struct
  let int = (+)
  let float = (+.)
end

let () = assert ([%imp Add] 1 2 = 3)
let () = assert ([%imp Add] 1.2 3.4 = 4.6)







(* XXX
let add = [%imp Add]

Error: No instance found for
*uniq*1
*)







let add ~_d:(_d:('a, [%imp Add]) Leopard.Implicits.t) = Leopard.Implicits.get ~_d

(* =>
let add ~_d:((_d : ('a, [ `Add ]) Leopard.Implicits.t) as __imp__arg__1)  =
  Leopard.Implicits.get ~_d
*)





(* XXX
let add ~_d:(_d:('a, 'b) Leopard.Implicits.t) = Leopard.Implicits.get ~_d

=>
Error: This type 'b is where a type of encoded string was expected 
({id=854;level=100000000;desc= Tvar "b"})

Hmm, this is not very very required...
*)




let add ~_d = [%imp Add] ~_d
(* =>
   
let add ~_d:(_d as __imp__arg__2)  =
  (Leopard.Implicits.get : _d:('imp__any_var3, [ `Add ]) Leopard.Implicits.t
                             -> 'imp__any_var3) ~_d:((_d)[@imp_applied ])
*)




(* XXX
let add ~_d:(_ : ('a, [%imp Add]) Leopard.Implicits.t) = [%imp Add]
*)


