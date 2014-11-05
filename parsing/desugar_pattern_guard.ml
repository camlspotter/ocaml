open Asttypes
open Parsetree
open Ast_helper
open Ppxx

(*
In Pure OCaml with extension
[%pattern_guard
  e;;              for when
  let p = e;;      for  with p <- e
]

a ::= -> e
    | when e ; a
    | with p = e ; a

desugar (-> e) = 
    res := e; true

desugar (when e ; a) =
    e && e'
    where
      e' = desugar a

desugar (with p = e ; a) =
    (match e with p -> e' | _ -> false)
    where
      e' = desugar a

At the head,
let res = ref (Obj.magic 0 : 'a) in

*)

(* The above desugaring use a reference, therefore it is not thread-safe.
   The following is a thread-safe version.

desugar (-> e) = 
    raise (M.ExitWith e)

desugar (when e ; a) =
    e && e'
    where
      e' = desugar a

desugar (with p = e ; a) =
    (match e with p -> e' | _ -> false)
    where
      e' = desugar a

The pattern match must be surrounded by

function cases =>
  fun x ->
    let module M = struct exception ExitWith of t in
    try
      match x with
      cases'
    with
    | M.ExitWith v -> v
*)

(* CR jfuruse: do we really need to care about the thread safety?!?! *)

   
let need_desugar case =
  match case.pc_guard with
  | Some { pexp_desc = Pexp_extension ({txt="guard"}, PStr sitems)} -> 
      Some (List.map (function
        | { pstr_desc= Pstr_eval (e, _) } -> `When e
        | { pstr_desc= Pstr_value (_, [vb])} -> `With (vb.pvb_pat, vb.pvb_expr)
        | _ -> assert false)
              sitems)
  | _ -> None

let phx = "$phx"
let exp_phx = Exp.id phx
let phm = "$PH"
let id_phm_ExitWith = lid ?loc:None & phm ^ ".ExitWith"

let desugar_guard tv e_action gs = 
  let open Exp in
  let rec desugar = function
    | [] -> 
        with_default_loc e_action.pexp_loc & fun () ->
          raise_ (phm ^ ".ExitWith") & Some (magic & constraint_ e_action tv)
    | `When e :: gs ->
        with_default_loc e.pexp_loc & fun () ->
          apply (pervasives "&&")
            [ "", e;
              "", desugar gs ]
    | `With (p, e) :: gs ->
        with_default_loc (Location.merge p.ppat_loc e.pexp_loc) & fun () ->
          match_ e 
            [ { pc_lhs = p;
                pc_guard = Some (bool true); (* stupid but we need it to prevent the following "any" case does not produce Warning 11 *)
                pc_rhs = desugar gs }
            ; { pc_lhs = Pat.any ();
                pc_guard = None;
                pc_rhs = bool false } 
            ]
  in
  desugar gs

let desugar_case tv case guards_opt =
  match guards_opt with
  | None -> case
  | Some guards ->
      let guard = desugar_guard tv case.pc_rhs guards in
      { case with
        pc_guard = Some guard;
        pc_rhs = Exp.assert_false ~loc:guard.pexp_loc ()
      }

let desugar_cases loc build e cases =
  let open Exp in
  let guards_opt = List.map need_desugar cases in
  if List.for_all (fun x -> x = None) guards_opt then None (* no need *)
  else 
    with_default_loc loc & fun () ->
      let tv = Typ.new_var "p_g_" in
      let cases = List.map2 (desugar_case tv) cases guards_opt in
      let e =   
        letmodule (mkloc phm) 
          (Mod.structure [Str.exception_ { pext_name = mkloc "ExitWith";
                                           pext_kind = Pext_decl ([Typ.int ()], None);
                                           pext_loc = Location.none;
                                           pext_attributes = [] }])
          (try_ (build e cases)
             [ { pc_lhs = Pat.construct id_phm_ExitWith & Some (Pat.var "v");
                 pc_guard = None;
                 pc_rhs = magic & id "v" } ])
      in
      Some e
        
let desugar_expr exp = 
  let open Exp in
  let loc = exp.pexp_loc in
  let build_help f = f ?loc:(Some exp.pexp_loc) ?attrs:(Some exp.pexp_attributes) in
  match exp.pexp_desc with
  | Pexp_match (e, cases) ->
      begin match desugar_cases loc (build_help match_) e cases with
      | None -> exp
      | Some exp -> exp
      end
  | Pexp_function cases ->
      begin match desugar_cases loc (build_help match_) exp_phx cases with
      | None -> exp
      | Some exp -> fun_ "" None (Pat.var phx) exp
      end
  | Pexp_try (e, cases) ->
      begin match desugar_cases loc (build_help try_) e cases with
      | None -> exp
      | Some exp -> exp
      end
  | _ -> exp

open Ast_mapper

let extend super =
  let expr self e = super.expr self & desugar_expr e in 
  { super with expr }
