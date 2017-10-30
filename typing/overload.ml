(* open Compilerlib  *)
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

let scrape_sg env mdecl = 
  try
    match Env.scrape_alias env @@ Mtype.scrape env mdecl.Types.md_type with
    | Mty_signature sg -> sg
    | Mty_functor _ -> [] (* We do not scan the internals of functors *)
    | _ -> assert false
  with
  | e -> 
      Format.eprintf "scraping failed: %s" @@ Printexc.to_string e;
      raise e
  
let fold_module env path =
  Format.eprintf "fold_module %a@." Printtyp.path path;
  let mdecl = Env.find_module path env in
  let mty = mdecl.Types.md_type in
  let sg : Types.signature = scrape_sg env mdecl in
  let id = Ident.create "Dummy" in
  let env' = Env.add_module id mty Env.empty in
  List.fold_left (fun () -> function
      | Sig_module (id, _mdecl, _rs) ->
          let p = Env.lookup_module ~load:false Longident.(Ldot (Lident "Dummy", id.Ident.name)) env' in
          Format.eprintf "SUBMOD: %a@." Printtyp.path p;
          ()
      | _ -> ()) () sg
  
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
  Format.eprintf "resolve_overloading %a %a@." !Location.printer loc Printtyp.path path;
  let env = exp.exp_env in

  let name = get_name path in

  let rec find_candidates (path : Path.t) mty =
    (* Format.eprintf "Find_candidates %a@." Printtyp.path path; *)

(*
    let env = Env.empty in
*)
    let sg = 
      match Env.scrape_alias env @@ Mtype.scrape env mty with
      | Mty_signature sg -> sg
      | _ -> assert false
    in
    List.fold_right (fun sitem st -> match sitem with
    | Sig_value (id, _vdesc) when Ident.name id = name -> 
        let lident = Longident.Ldot (Untypeast.lident_of_path path, Ident.name id) in
        let path, vdesc = Env.lookup_value lident env  in
        if test env exp.exp_type vdesc then (path, vdesc) :: st else st
    | Sig_module (id, _mty, _) -> 
        let lident = Longident.Ldot (Untypeast.lident_of_path path, Ident.name id) in
        let path = Env.lookup_module ~load:true (*?*) lident env  in
        let moddecl = Env.find_module path env in
        find_candidates path moddecl.Types.md_type @ st
    | _ -> st) sg []
  in

  let lid_opt = match path with
    | Path.Pident _ -> None
    | Path.Pdot (p, _, _) -> Some (Untypeast.lident_of_path p)
    | Path.Papply _ -> assert false
  in

  let mpath = match path with
    | Path.Pident _ -> assert false (* must be fixed XXX *)
    | Path.Pdot (p, _, _) -> p
    | Path.Papply _ -> assert false
  in

  match
    fold_module env mpath;
    prerr_endline "ha";
    (* Here Env.empty must be used! ... Really!??!  How about local overloading? *)
    Format.eprintf "%s@." (match lid_opt with None -> "none" | Some l -> String.concat "." (Longident.flatten l));
    Env.fold_modules (fun _name path moddecl st -> 
        Format.eprintf "%s %a@." _name Printtyp.path path;
        find_candidates path moddecl.Types.md_type @ st) lid_opt env []
  with
  | [] -> 
     Location.raise_errorf ~loc:lidloc.loc "Overload resolution failed: no match"
  | [path, vdesc] -> 
      (* Format.eprintf "RESOLVED: %a@." print_path path; *)
      let ity = Ctype.instance env vdesc.val_type in
      Ctype.unify env exp.exp_type ity; (* should succeed *)
      Unshadow.Replace.replace 
        { exp with 
          exp_desc = Texp_ident (path, {lidloc with Asttypes.txt = Untypeast.lident_of_path path}, vdesc);
          exp_type = exp.exp_type }
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
  if !Leopardtype.overload then
    Unshadow.Alias.insert @@ Map.map_structure str
  else str
