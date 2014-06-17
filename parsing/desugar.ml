(* desugaring of Haskellish value-type declaration *)
open Asttypes
open Parsetree

let filter_let_binding = function 
  | { pvb_pat = 
        { ppat_desc = Ppat_constraint( { ppat_desc= Ppat_var sloc }, ty);
          ppat_loc = loc
        };
      pvb_attributes = (({ txt = "haskellish" }, PStr []) :: _);
      (* pvb_loc = symbol_rloc() *) 
    } -> 
      `Left (sloc.txt, (sloc.loc, ty, loc))
      
  | x -> `Right x

let desugar tbl = 
  let open Ast_mapper in
  let super = default_mapper in
  { super with
    pat = fun self pat ->
      match pat.ppat_desc with
      | Ppat_var {txt} -> 
          begin match List.assoc txt tbl with
          | (_vloc, ty, patloc) -> 
              { ppat_desc = Ppat_constraint (pat, ty);
                ppat_loc = patloc;
                ppat_attributes = [] }
          | exception Not_found -> pat
          end
      | _ -> super.pat self pat
  }

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

type error =
  | Same_variable_declared_twice of string * Location.t * Location.t

exception Error of error

let desugar_let_bindings lbs =
  let (haskellishes, lbs) = list_partition_map filter_let_binding lbs in

  (* sanity checks: no variable is declared twice *)
  let rec check st = function
    | [] -> ()
    | (v, (vloc, _, _) as x)::xs -> 
        begin match List.assoc v st with
        | (vloc', _, _) ->
            raise (Error (Same_variable_declared_twice (v, vloc, vloc')))
        | exception Not_found -> 
            check (x::st) xs
        end
  in
  check [] haskellishes;

  (* CR jfuruse: check: all the haskellish things are used *)

  let mapper = desugar haskellishes in
  List.map (fun vb ->
    { vb with pvb_pat = mapper.Ast_mapper.pat mapper vb.pvb_pat }) lbs
