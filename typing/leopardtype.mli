val without_leopard : (unit -> 'a) -> 'a
(** [without_leopard f] runs [f] disabling all the features of leopard *)

val curried_constr : bool ref
val overload : bool ref

(** Typing tools *)
    
val scrape_sg : Env.t -> Types.module_declaration -> Types.signature

val fold_module
  : Env.t
  -> Path.t
  -> 'a ->
  ('a
   -> [> `Class   of Ident.t * Path.t * Types.class_declaration
       | `Cltype  of Ident.t * Path.t * Types.class_type_declaration
       | `Modtype of Ident.t * Path.t * Types.modtype_declaration
       | `Module  of Ident.t * Path.t * Types.module_declaration
       | `Type    of Ident.t * Path.t * Types.type_declaration
       | `Typext  of Ident.t * Path.t * Types.extension_constructor
       | `Value   of Ident.t * Path.t * Types.value_description ]
   -> 'a)
  -> 'a

