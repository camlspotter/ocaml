(* desugaring of Haskellish value-type declaration *)
open Asttypes
open Parsetree
open Syntaxerr
open Ast_mapper
open Ppxx

let list_partition_map f xs =
  let rec aux (lst, rst) = function
    | [] -> List.rev lst, List.rev rst
    | x::xs ->
        let st' = match f x with
          | `Left v -> (v::lst, rst)
          | `Right v -> (lst, v::rst)
        in
        aux st' xs
  in
  aux ([],[]) xs

let filter_let_binding = function 
  | { pvb_pat = 
        { ppat_desc = Ppat_constraint( { ppat_desc= Ppat_var sloc }, ty);
          ppat_loc = loc
        };
      pvb_expr = { pexp_desc = Pexp_extension ( {txt="val"}, PStr []) } } ->
      (* [let f : typ = [@val]] *)
      `Left (sloc.txt, (sloc.loc, ty, loc, ref false (* used or not *)))
  | { pvb_expr = { pexp_desc = Pexp_extension ( {txt="val"}, PStr []) } } ->
      assert false (* The PVB is not in the form of [x : ty] *)
  | { pvb_expr = { pexp_desc = Pexp_extension ( {txt="val"}, _) } } ->
      assert false (* [%val] cannot have any payloads *)
  | x -> `Right x

let add_constraints tbl = 
  let super = default_mapper in
  let mapper = 
    { super with
      pat = fun self pat ->
        match pat.ppat_desc with
        | Ppat_var {txt} -> 
            begin match List.assoc txt tbl with
            | (_vloc, ty, patloc, used) -> 
                used := true;
                { ppat_desc = Ppat_constraint (pat, ty);
                  ppat_loc = patloc;
                  ppat_attributes = [] }
            | exception Not_found -> pat
            end
        | _ -> super.pat self pat
    }
  in
  mapper.pat mapper

let desugar_value_bindings vbs =
  let (haskellishes, vbs) = list_partition_map filter_let_binding vbs in

  (* sanity checks: no variable is declared twice *)
  let rec check st = function
    | [] -> ()
    | (v, (vloc, _, _, _) as x)::xs -> 
        begin match List.assoc v st with
        | (vloc', _, _, _) ->
            raise (Error (Desugar_same_declared_twice(vloc', vloc, v)))
        | exception Not_found -> 
            check (x::st) xs
        end
  in
  check [] haskellishes;

  let pat_map = add_constraints haskellishes in

  let vbs = List.map (fun vb -> 
    { vb with pvb_pat = pat_map vb.pvb_pat }) vbs in

  (* check: all the haskellish declarations are used? *)
  List.iter (fun (v, (vloc, _, _, used)) ->
    if !used = false then
      raise (Error (Desugar_declaration_is_never_used (vloc, v)))
  ) haskellishes;

  vbs

let desugar_expr exp = 
  match exp.pexp_desc with
  | Pexp_let (rf, vbs, e) ->
      { exp with pexp_desc = Pexp_let (rf, desugar_value_bindings vbs, e) }
  | _ -> exp

let extend super =
  let expr self e = super.expr self & desugar_expr e in
  { super with expr }

let () = Ast_mapper.extend_builtin_mapper extend
