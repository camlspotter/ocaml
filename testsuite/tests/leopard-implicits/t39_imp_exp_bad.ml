let add ~_d:(_ : ('a, [%imp Add]) Leopard.Implicits.t) = [%imp Add]
(* This must be rejected, since 

     _d has type  ('a, [%imp Add]) Leopard.Implicits.t

     [%imp Add] requires  ('b, [%imp Add]) Leopard.Implicits.t,  'b is a diffrent type from 'a
   
     _d cannot provide the value for [%imp Add]
   
*)
   
