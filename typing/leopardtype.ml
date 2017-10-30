let curried_constr = ref true
let overload = ref true

let enable_leopard () =
  curried_constr := true;
  overload := true
    
let disable_leopard () =
  curried_constr := false;
  overload := false
    
let without_leopard f =
  disable_leopard ();
  match f () with
  | exception e ->
      enable_leopard ();
      raise e
  | res ->
      enable_leopard ();
      res

open Types

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
  
      

