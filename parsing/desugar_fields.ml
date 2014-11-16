(*
open Asttypes
*)
open Ppxx (* must come after Ast_helper *)
open Ast_mapper
open Parsetree
open Longident
open Location

let is_id name e = match e.pexp_desc with
  | Pexp_ident {txt = Lident n} ->
      name = n && e.pexp_attributes = [] 
  | _ -> false

let desugar_expr e = 
  let open Exp in
  match e.pexp_desc with
  | Pexp_field (e', lid) when is_id "!" e' ->
      (* (!).l => fun x -> x.l *)
      let loc = Location.ghost e'.pexp_loc in
      let xe, xp = ExpPat.var ~loc "x" in
      fun_ ~loc "" None xp & with_desc e & Pexp_field (xe, lid)

  | Pexp_setfield (e', lid, e'') when is_id "!" e' ->
      (* (!).l <- e => fun x -> x.l <- e *)
      let loc = Location.ghost e'.pexp_loc in
      let xe, xp = ExpPat.var ~loc "x" in
      fun_ ~loc "" None xp & with_desc e & Pexp_setfield (xe, lid, e'')

  | Pexp_field (e', lid) when is_id "!<-" e' ->
      (* (!<-).l => fun x e -> x.l <- e *)
      let loc = Location.ghost e'.pexp_loc in
      let xe, xp = ExpPat.var ~loc "x" in
      let ee, ep = ExpPat.var ~loc "e" in
      fun_ ~loc "" None xp 
      & fun_ ~loc "" None ep 
      & with_desc e & Pexp_setfield (xe, lid, ee)

  | Pexp_send (e', m) when is_id "!" e' ->
      (* (!)#m => fun x -> x#l *)
      let loc = Location.ghost e'.pexp_loc in
      let xe, xp = ExpPat.var ~loc "x" in
      fun_ ~loc "" None xp & with_desc e & Pexp_send (xe, m)

  | _ -> e

let extend super =
  let expr self e = super.expr self & desugar_expr e in
  { super with expr }
