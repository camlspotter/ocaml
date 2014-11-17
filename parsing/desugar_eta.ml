open Asttypes
open Parsetree
open Ppxx

let is_simple_ext s e = match e.pexp_desc with
  | Pexp_extension ({ txt }, PStr []) -> txt = s
  | _ -> false

let desugar_expr exp = 
  let open Exp in
  match exp.pexp_desc with
  | Pexp_apply(e, xs) when is_simple_ext "eta" e ->
      begin match xs with
      | [] -> assert false
      | ("", e1)::xs ->
          (* [%eta] e1 xs  ==>  fun x -> e xs x *)
          let loc_fun = Location.ghost exp.pexp_loc in
          let loc_x = Location.ghost e.pexp_loc in
          let xe, xp = ExpPat.var ~loc:loc_x "x" in
          fun_ ~loc:loc_fun "" None 
            xp { exp with pexp_desc = Pexp_apply (e1, xs @ [("", xe)]) }
      | _ -> assert false (* first arg cannot have a label *)
      end
  | _ -> exp

open Ast_mapper

let extend super =
  let expr self e = super.expr self & desugar_expr e in 
  { super with expr }
