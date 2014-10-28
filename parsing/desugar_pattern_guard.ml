open Asttypes
open Ast_helper
open Longident
open Parsetree
open Ast_helper.Exp

let txt x = { txt = x; loc = Location.none }

module LI = struct
  let ( ! ) x = Lident x
  let ( * ) x y = Ldot (x, y)
  let (@) x y = Lapply (x, y)
end

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
  
let ph = "$ph"
let exp_ph = ident (txt @@ LI.(!ph))
let phx = "$phx"
let exp_phx = ident (txt @@ LI.(!ph))
let phm = "$PH"
let phm_ExitWith = LI.(!phm * "ExitWith")

let tvar = 
  (* pity we need a counter *)
  let cntr = ref 0 in
  fun () ->
    incr cntr;
    Typ.var ("p_g_" ^ string_of_int !cntr)

let exp_bool b =
  construct (txt @@ LI.(!(if b then "true" else "false"))) None

let exp_pervasives s =
  ident (txt @@ LI.(!"Pervasives" * s))

let apply_obj_magic e =
  apply (ident (txt @@ LI.(!"Obj" * "magic"))) ["", e]

let desugar_guard tv e_action gs = 
  let rec desugar = function
    | [] -> 
        apply (exp_pervasives "raise")
          [ "", 
            construct (txt @@ LI.(!phm * "ExitWith")) 
              (Some (apply_obj_magic @@ constraint_ e_action tv))
          ]
    | `When e :: gs ->
        apply (ident (txt @@ LI.(!"Pervasives" * "&&")))
          [ "", e;
            "", desugar gs ]
    | `With (p, e) :: gs ->
        match_ e 
          [ { pc_lhs = p;
              pc_guard = Some (exp_bool true); (* stupid but we need it to prevent the following "any" case does not produce Warning 11 *)
              pc_rhs = desugar gs }
          ; { pc_lhs = Pat.any ();
              pc_guard = None;
              pc_rhs = exp_bool false } 
          ]
  in
  desugar gs

let desugar_case tv case guards_opt =
  match guards_opt with
  | None -> case
  | Some guards ->
      { case with
        pc_guard = Some (desugar_guard tv case.pc_rhs guards);
        pc_rhs = assert_ (exp_bool false)
      }

let desugar_cases build e cases =
  let guards_opt = List.map need_desugar cases in
  if List.for_all (fun x -> x = None) guards_opt then None (* no need *)
  else 
    let tv = tvar () in
    let cases = List.map2 (desugar_case tv) cases guards_opt in
    let e =   
      letmodule (txt phm) 
        (Mod.structure [Str.exception_ { pext_name = txt "ExitWith";
                                         pext_kind = Pext_decl ([Typ.constr (txt @@ LI.(!"int")) []], None);
                                         pext_loc = Location.none;
                                         pext_attributes = [] }])
        (try_ (build e cases)
           [ { pc_lhs = Pat.construct (txt phm_ExitWith) @@ Some (Pat.var (txt "v"));
               pc_guard = None;
               pc_rhs = apply_obj_magic @@ ident (txt @@ LI.(!"v")) } ])
    in
    Some e
        
let desugar_expr exp = 
  let build_help f = f ?loc:(Some exp.pexp_loc) ?attrs:(Some exp.pexp_attributes) in
  match exp.pexp_desc with
  | Pexp_match (e, cases) ->
      begin match desugar_cases (build_help match_) e cases with
      | None -> exp
      | Some exp -> exp
      end
  | Pexp_function cases ->
      begin match desugar_cases (build_help match_) exp_phx cases with
      | None -> exp
      | Some exp -> fun_ "" None (Pat.var (txt phx)) exp
      end
  | Pexp_try (e, cases) ->
      begin match desugar_cases (build_help try_) exp_phx cases with
      | None -> exp
      | Some exp -> fun_ "" None (Pat.var (txt phx)) exp
      end
  | _ -> exp
