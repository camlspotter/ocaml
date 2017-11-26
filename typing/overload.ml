(* open Compilerlib  *)
open Leopardparsing
open Leopardtyping
open Asttypes
open Types
open Typedtree
    
(*
let print_ident ppf id = Format.fprintf ppf "%s/%d" id.Ident.name id.Ident.stamp

let rec print_path ppf = function
  | Path.Pident id -> print_ident ppf id
  | Path.Pdot (p, name, n) -> Format.fprintf ppf "%a.%s__%d" print_path p name n
  | Path.Papply (p1, p2) -> Format.fprintf ppf "%a(%a)" print_path p1 print_path p2
*)

let get_name = function
  | Path.Pident id -> Ident.name id
  | Path.Pdot (_, name, _) -> name
  | Path.Papply _ -> assert false

let test env ty vdesc =
  let snapshot = Btype.snapshot () in
  let ity = Ctype.instance env vdesc.val_type in
  let res = try  Ctype.unify env ty ity; true with _ -> false in
  Btype.backtrack snapshot;
  res

let resolve_overloading exp ({loc} as lidloc) path = 
(*
  Format.eprintf "resolve_overloading %a %a@." !Location.printer loc Printtyp.path path;
*)
  match Types.gen_vars exp.exp_type with
  | _::_ ->
      Location.raise_errorf ~loc "@[<2>Overloaded values cannot be used in polymorphic contexts,@ but this value is used as@ %a@]" Printtyp.type_scheme exp.exp_type
  | [] ->

  let env = exp.exp_env in

  let name = get_name path in

  let rec find_candidates env (path : Path.t) =
    (* Format.eprintf "Find_candidates %a@." Printtyp.path path; *)
    let res = Env.fold_module env path [] @@ fun st -> function
    | Env.Value (id, path, vdesc) when Ident.name id = name -> 
        if test env exp.exp_type vdesc then (path, vdesc) :: st else st
    | Env.Module (_id, path, _moddecl) ->
        find_candidates env path @ st
    | _ -> st
    in
    (* Format.eprintf "Find_candidates %a done@." Printtyp.path path;*)
    res
  in

  let mpath = match path with
    | Path.Pident _ ->
        (* Using an overloaded value in the same module where it is defined
           is not allowed, since it is hard to get the instance search space.
        *)
        Location.raise_errorf ~loc "Overload value cannot be used in the module where it is defined"
    | Path.Pdot (p, _, _) -> p
    | Path.Papply _ -> assert false
  in

  match
    (* Here Env.empty must be used! ... Really!??!  How about local overloading? *)
    (* Format.eprintf "Checking %a@." Path.format mpath; *)
    Env.fold_module env mpath [] @@ fun st -> function
    | Env.Module (_id, path, _) ->
        (* Format.eprintf "%s %a@." name Printtyp.path path; *)
        find_candidates env path @ st
    | _ -> st
  with
  | [] -> 
     Location.raise_errorf ~loc:lidloc.loc "Overload resolution failed: no match"
  | [path, vdesc] -> 
      (* Format.eprintf "RESOLVED: %a@." print_path path; *)
      let ity = Ctype.instance env vdesc.val_type in
      Ctype.unify env exp.exp_type ity; (* should succeed *)
      (* We do not try unshadowing, since
           * It is complicated.
           * It does not work in toplevel.

         If the resolved value is shadowed and not accessible trivially,
         we simply reject the expression.
      *)
      begin match Env.value_accessibility env path with
      | `Accessible lid ->
          { exp with 
            exp_desc = Texp_ident (path, {lidloc with Asttypes.txt = lid}, vdesc);
            exp_type = exp.exp_type }
      | `ShadowedBy (vm, p, loc) ->
          Location.raise_errorf ~loc:lidloc.loc
            "@[<2>Overloading is resolved to %a,@ which is shadowed by %s %a@ defined at@ %a@]"
            Path.format path
            (match vm with `Value -> "value" | `Module -> "module")
            Path.format p
            Location.format loc
      | `NotFound ->
          Format.eprintf "value_accessibility: NotFound for %a@." Path.format path;
          assert false
      end

  | _ -> 
     Location.raise_errorf ~loc:lidloc.loc "Overload resolution failed: too ambiguous"

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  let enter_expression = function
    | ({ exp_desc= Texp_ident (path, lidloc, vdesc) } as e)-> 
        begin match vdesc.val_kind with
        | Val_prim { Primitive.prim_name = "%OVERLOADED" } ->
           resolve_overloading e lidloc path
        | _ -> e
        end
    | e -> e
end

module Map = TypedtreeMap.MakeMap(MapArg)

let resolve str =
  if !Leopardfeatures.overload then Map.map_structure str
  else str
