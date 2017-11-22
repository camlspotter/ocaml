open List
open Ident
open Path
open Location

(* tools *)

let rev_filter_map f lst =
  fold_left (fun st x ->
    match f x with
    | Some v -> v :: st
    | None -> st) [] lst

(** mapMaybe of Haskell *)
let filter_map f lst = rev (rev_filter_map f lst)

let flip f x y = f y x
  
let conv_ident id = "__" ^ id.name ^ "__" ^ string_of_int id.stamp

let aliases = ref ([]: (Ident.t * Ident.t) list)
(* CR jfuruse: This is bizarre to have a ref which none of this module touches... *)

let reset () = aliases := []

let rec check_module_path env path =
  (* Format.eprintf "  checking %a@." Printtyp.path_verbose path; *)
  let lid = Untypeast.lident_of_path path in
  let path' = try Some (Env.lookup_module ~load:true (* CR jfuruse: ? *) lid env) with _ -> None in 
  if Some path = path' then `Accessible lid
  else begin
  (* Format.eprintf "    shadowed: found %a@." (Option.format Printtyp.path_verbose) path'; *)
    match path with
    | Pident id ->
        let id' = match List.assoc id !aliases with
          | exception Not_found ->
              let n = conv_ident id in
              let id' = Ident.create n in
              aliases := (id,id') :: !aliases;
              id'
          | id' -> id'
        in
        `Shadowed (id, id', Pident id')
    | Pdot (p, n, x) -> 
        begin match check_module_path env p with
        | `Shadowed (id, name, p) -> `Shadowed (id, name, Pdot (p, n, x))
        | `Accessible _ -> assert false (* impos *)
        | `Not_found p -> `Not_found p
    end
    | _ -> assert false (* impos *)
  end

module Replace = struct
  module MapArg = struct
    include TypedtreeMap.DefaultMapArgument
    
    open Typedtree
    
    (* module Forge = Forge *)
    
    let enter_expression e = match e.exp_desc with
      | Texp_ident (p, {loc}, vd) ->
          let env = e.exp_env in
          begin match p with
          | Path.Pdot (p, n, i) ->
              begin match check_module_path env p with
              | `Accessible _ -> e
              | `Not_found _p ->  assert false (* impos *)
              | `Shadowed (_id, _id', p) ->
                  let p = Path.Pdot (p, n, i) in
                  let lid = Untypeast.lident_of_path p in
                  { e with exp_desc = Texp_ident (p, {txt=lid; loc}, vd) }
              end
          | Path.Pident _ ->
              let lid = Untypeast.lident_of_path p in
              begin
                try
                  let p', _ = Env.lookup_value lid env in
                  assert (p = p');
                  e
                with Not_found -> e
              end
          | _ -> assert false (* impos *)
          end
      | _ -> e
  end
  
  module Map = TypedtreeMap.MakeMap(MapArg)

  let replace e = Map.map_expression e
end
  
module Alias = struct

  open Typedtree
      
  let build_mexp env id mty =
    let p = Pident id in
    let lid = Untypeast.lident_of_path p in
    { mod_desc = Tmod_ident (p, {txt=lid; loc= Location.none})
    ; mod_loc = Location.none
    ; mod_type = mty
    ; mod_env = env
    ; mod_attributes = []
    }

  let build_str_item env id' mexp =
    { str_desc = Tstr_module { mb_id = id'
                             ; mb_name = {txt=id'.Ident.name; loc=Location.none}
                             ; mb_expr = mexp
                             ; mb_attributes = []
                             ; mb_loc = Location.none }
    ; str_loc = Location.none
    ; str_env = env
    }  

  module MapArg(A : sig val aliases : (Ident.t * Ident.t) list end) : TypedtreeMap.MapArgument = struct
  
    (* Introduce module aliases to avoid shadow
  
       module M = ...
       
       =>
  
       module M___ = M
       module M = ...
  
       or
  
       let module M = ... in
       
       =>
  
       let module M___ = M in
       let module M = ... in
    *)
    include TypedtreeMap.DefaultMapArgument
  
    open Typedtree

    let enter_expression e = match e.exp_desc with
      | Texp_letmodule (id, a, b, e') ->
          begin match assoc_opt id A.aliases with
          | None -> e
          | Some id' ->
              let mexp = build_mexp e.exp_env id b.mod_type in
              { e with exp_desc = Texp_letmodule (id, a, b,
                { e with exp_desc = Texp_letmodule (id', {txt=id'.Ident.name; loc= Location.none}, mexp, e')})} 
          end
      | _ -> e
        
    let structure_item si = match si.str_desc with
      | Tstr_module mb ->
          (*
            Format.eprintf "module %a@." Ident.format mb.mb_id;
          *)
          si ::
          begin match assoc_opt mb.mb_id A.aliases with
          | None -> []
          | Some id' -> 
              let mexp = build_mexp si.str_env mb.mb_id mb.mb_expr.mod_type in
              [ build_str_item si.str_env id' mexp ]
          end
      | Tstr_recmodule mbs ->
          si :: flip filter_map mbs (fun mb ->
            match assoc_opt mb.mb_id A.aliases with
            | None -> None
            | Some id' -> 
                let mexp = build_mexp si.str_env mb.mb_id mb.mb_expr.mod_type in
                Some (build_str_item si.str_env id' mexp))
      | _ -> [si]
  
    let enter_structure ({ str_items = sis } as s) =
      (* It ignores the correctness of type information *)
      { s with str_items = fold_right (fun si st -> structure_item si @ st) sis [] }
  end
  
  let insert str =
    match !aliases with
    | [] -> str
    | als ->
        aliases := [];
        let module A = MapArg(struct let aliases = als end) in
        let module Map = TypedtreeMap.MakeMap(A) in
        let add_persistent str =
          List.fold_left (fun str (id,id') ->
              if Ident.persistent id then
                let env = str.str_final_env in
                let md = Env.find_module (Path.Pident id) env in
                let mexp = build_mexp env id md.Types.md_type in
                { str with str_items = build_str_item env id' mexp :: str.str_items }
              else str) str als
        in
        add_persistent @@ Map.map_structure str
end
