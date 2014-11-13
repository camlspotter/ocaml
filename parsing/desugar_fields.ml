open Asttypes
open Parsetree
open Ppxx
open Longident

(* record/method accessors are purely syntactic sugar

(!).label      => fun x -> x.label
(!).label <- e => fun x -> x.label <- e
(!<-).label    => fun x e -> x.label <- e
(!)#meth       => fun x -> x#meth

*)

let is_id name e = match e.pexp_desc with
  | Pexp_ident {txt=Lident n} when name = n -> 
      e.pexp_attributes = [] (* (!)[@x] is not considered as "bang" *)
  | _ -> false

let is_bang = is_id "!"

let desugar_expr e = 
  let open Exp in
  match e.pexp_desc with
  | Pexp_field (e', lid) when is_bang e' ->
      (* (!).label   =>   fun x -> x.label *)
      let loc = Location.ghost e'.pexp_loc in
      let xe, xp = ExpPat.var ~loc "x" in
      fun_ ~loc "" None xp { e with pexp_desc = Pexp_field(xe, lid) }
  | Pexp_setfield (e', lid, e'') when is_bang e' ->
      (* (!).label <- e''  =>   fun x -> x.label <- e'' *)
      let loc = Location.ghost e'.pexp_loc in
      let xe, xp = ExpPat.var ~loc "x" in
      fun_ ~loc "" None xp { e with pexp_desc = Pexp_setfield(xe, lid, e') }
  | Pexp_field (e', lid) when is_id "!<-" e' ->
      (* (!<-).label    => fun x e -> x.label <- e *)
      let loc = Location.ghost e'.pexp_loc in
      let xe, xp = ExpPat.var ~loc "x" in
      let ee, ep = ExpPat.var ~loc "e" in
      fun_ ~loc "" None xp 
        (fun_ ~loc "" None ep 
           { e with pexp_desc = Pexp_setfield(xe, lid, ee) })
  | Pexp_send (e', m) when is_bang e' ->
      (* (!)#meth    => fun x -> x#meth *)
      let loc = Location.ghost e'.pexp_loc in
      let xe, xp = ExpPat.var ~loc "x" in
      fun_ ~loc "" None xp 
        { e with pexp_desc = Pexp_send(xe, m) }
  | _ -> e

open Ast_mapper

let extend super =
  let expr self e = super.expr self & desugar_expr e in 
  { super with expr }

(* Actual extension is delayed.
let () = Ast_mapper.extend_builtin_mapper extend
*)
