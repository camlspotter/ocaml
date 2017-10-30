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
  
let fold_module env path init f =
  Format.eprintf "fold_module %a@." Printtyp.path path;
  let mdecl = Env.find_module path env in
  let mty = mdecl.Types.md_type in
  let sg : Types.signature = scrape_sg env mdecl in
  let id = Ident.create "Dummy" in
  let env' = Env.add_module id mty Env.empty in
  let lid id = Longident.(Ldot (Lident "Dummy", id.Ident.name)) in
  let pathfix p = match p with
    | Path.Pdot (Path.Pident _, s, n) -> Path.(Pdot (path, s, n))
    | _ -> assert false
  in
  List.fold_left (fun st i ->
      let x = match i with
        | Sig_value (id, _vdesc) ->
            let p, vdesc = Env.lookup_value (lid id) env' in
            `Value (id, pathfix p, vdesc)
        | Sig_type (id, td, _) ->
            let p = Env.lookup_type (lid id) env' in
            `Type (id, pathfix p, td)
        | Sig_typext (id, ec, _) ->
            let p = Env.lookup_type (lid id) env' in
            `Typext (id, pathfix p, ec)
        | Sig_module (id, mdecl, _) ->
            let p = Env.lookup_module ~load:false (lid id) env' in
            `Module (id, pathfix p, mdecl)
        | Sig_modtype (id, _) ->
            let p, mdtd = Env.lookup_modtype (lid id) env' in
            `Modtype (id, pathfix p, mdtd)
        | Sig_class (id, _, _) ->
            let p, cd = Env.lookup_class (lid id) env' in
            `Class (id, pathfix p, cd)
        | Sig_class_type (id, _, _) ->
            let p, ctd = Env.lookup_cltype (lid id) env' in
            `Cltype (id, pathfix p, ctd)
      in
      f st x) init sg
  
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

  let rec find_candidates env (path : Path.t) =
    (* Format.eprintf "Find_candidates %a@." Printtyp.path path; *)
    fold_module env path [] @@ fun st -> function
    | `Value (id, path, vdesc) when Ident.name id = name -> 
        if test env exp.exp_type vdesc then (path, vdesc) :: st else st
    | `Module (_id, path, _moddecl) ->
        find_candidates env path @ st
    | _ -> st
  in

  let mpath = match path with
    | Path.Pident _ -> assert false (* must be fixed XXX *)
    | Path.Pdot (p, _, _) -> p
    | Path.Papply _ -> assert false
  in

  match
    (* Here Env.empty must be used! ... Really!??!  How about local overloading? *)
    fold_module env mpath [] @@ fun st -> function
    | `Module (_id, path, _) ->
        Format.eprintf "%s %a@." name Printtyp.path path;
        find_candidates env path @ st
    | _ -> st
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
