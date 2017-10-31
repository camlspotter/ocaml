(* Extension of compilier_libs modules *)

open Leopardutils
open Asttypes
open List

module Ast_helper = struct
  include Ast_helper
  open Location
  let ghost l = { l with loc_ghost = true }
end

module Longident = struct
  include Longident

  open Format
  
  (* [Pprintast.longident] is not exposed in OCaml 4.05.0 *)
  let rec format ppf = function
    | Lident n -> pp_print_string ppf n
    | Ldot (p, name) -> fprintf ppf "%a.%s" format p name
    | Lapply (p1, p2) -> fprintf ppf "%a(%a)" format p1 format p2
                           
  let to_string l = asprintf "%a" format l
end

module Ident = struct
  include Ident

  open Format
  
  let format ppf id = pp_print_string ppf id.name
  let format_verbose ppf id = fprintf ppf "%s/%d" id.name id.stamp
end

module Path = struct
  include Path

  open Format

  let format = Printtyp.path

  let rec format_verbose ppf = function
    | Pident id -> Ident.format_verbose ppf id
    | Pdot (p, name, n) -> fprintf ppf "%a.%s__%d" format_verbose p name n
    | Papply (p1, p2) -> fprintf ppf "%a(%a)" format_verbose p1 format_verbose p2

  let to_string l = asprintf "%a" format l
end

module Location = struct
  include Location
  let merge t1 t2 = { t1 with loc_end = t2.loc_end }

  let format = print_loc
end

let raise_errorf = Location.raise_errorf
type 'a loc = 'a Location.loc = { txt : 'a; loc : Location.t }

module XParsetree = struct
  (* We cannot include Parsetree since it lacks implementation *)
  open Parsetree
    
  let iter_core_type f ty = match ty.ptyp_desc with
      Ptyp_any | Ptyp_var _ -> ()
    | Ptyp_arrow (_, ty1, ty2) -> f ty1; f ty2
    | Ptyp_tuple l      
    | Ptyp_constr (_, l)
    | Ptyp_class (_, l) -> iter f l
    | Ptyp_alias (ty, _) -> f ty
    | Ptyp_object(s_a_cty_l, _) ->
        iter (fun (_, _, cty) -> f cty) s_a_cty_l
    | Ptyp_variant (rfs, _, _) ->
        iter (function
          | Rtag (_, _, _, l) -> iter f l
          | Rinherit t -> f t) rfs
    | Ptyp_poly (_, t) -> f t
    | Ptyp_package (_, l_cty_s) ->
        iter (fun (_, t) -> f t) l_cty_s
    | Ptyp_extension _ -> ()

  module LongidentSet = Set.Make(struct type t = Longident.t let compare = compare end)

  (* referred constrs and classes *)   
  let constrs_in_core_type_ ty =
    let s = ref LongidentSet.empty in
    let add l = s := LongidentSet.add l !s in
    let rec f ty =
      begin match ty.ptyp_desc with
      | Ptyp_constr ({txt}, _) -> add txt
      | Ptyp_class ({txt}, _) -> add txt
      | _ -> ()
      end;
      iter_core_type f ty
    in
    f ty;
    !s

  let constrs_in_core_type ty =
    LongidentSet.elements & constrs_in_core_type_ ty

  let constrs_in_type_declaration td =
    constrs_in_core_type
    & Ast_helper.Typ.tuple
    & concat_map (fun (ty1, ty2, _) -> [ty1; ty2]) td.ptype_cstrs
    @ begin match td.ptype_kind with
      | Ptype_abstract -> []
      | Ptype_variant cds ->
          concat_map (fun cd -> 
            (match cd.pcd_args with
            | Pcstr_tuple ctys -> ctys
            | Pcstr_record lds -> map (fun x -> x.pld_type) lds)
            @ Option.to_list cd.pcd_res) cds
      | Ptype_record ldl ->
          map (fun ld -> ld.pld_type) ldl
      | Ptype_open -> []
      end
    @ Option.to_list td.ptype_manifest

  (* XXX should be replaced by Strongly_connected_components *)
  let sccs (es : ('v * 'v list) list) : 'v list list =
    match es with
    | [] -> []
    | _ -> 
    let rec f cntr vns s p sccs (v : 'v * 'v list) =
      let (v_, w_s) = v in
      let vns = (v_,cntr) :: vns in
      let s = v :: s in
      let p = (v,cntr) :: p in
      let cntr = cntr + 1 in
      let cntr, vns, s, p, sccs =
        fold_left (fun (cntr, vns, s, p, sccs) w_ ->
          let w = w_, assoc w_ es in
          match assoc_opt w_ vns with
          | None -> f cntr vns s p sccs w
          | Some n ->
              let rec pop = function
                | ((_,n')::_ as p) when n' <= n -> p
                | _::vns -> pop vns
                | [] -> assert false
              in
              cntr, vns, s, pop p, sccs) (cntr, vns, s, p, sccs) w_s
      in
      match p with
      | [] -> assert false
      | ((v'_,_),_) :: p when v_ = v'_ ->
          let rec pop scc = function
            | (v'_,_)::s ->
                if v_ = v'_ then (v'_::scc), s
                else pop (v'_::scc) s
            | _ -> assert false
          in
          let scc, s = pop [] s in
          cntr, vns, s, p, scc::sccs
      | _ -> cntr, vns, s, p, sccs
    in
    let _, _, _, _, sccs = f 0 [] [] [] [] (List.hd es) in
    sccs

  let group_type_declarations tds =
    let names = List.map (fun td -> td.ptype_name.txt) tds in
    let alist = List.map (fun td -> td.ptype_name.txt, td) tds in
    let mutually_defined td =
      filter_map (function Longident.Lident s when List.mem s names -> Some s | _ -> None)
      & constrs_in_type_declaration td
    in
    let graph, nonrecs = List.partition_map (fun td ->
      match mutually_defined td with
      | [] -> `Right td.ptype_name.txt
      | ns -> `Left (td.ptype_name.txt, ns)) tds
    in
    let groups = sccs graph in
    (List.map (List.map (flip List.assoc alist)) groups,
     List.map (flip List.assoc alist) nonrecs)
      
  let is_gadt type_decl = match type_decl.ptype_kind with
    | Ptype_variant constrs -> List.exists (fun c -> c.pcd_res <> None) constrs
    | _ -> false
end

module Ctype = struct
(*
  ctype.ml says:

   Type manipulation after type inference
   ======================================
   If one wants to manipulate a type after type inference (for
   instance, during code generation or in the debugger), one must
   first make sure that the type levels are correct, using the
   function [correct_levels]. Then, this type can be correctely
   manipulated by [apply], [expand_head] and [moregeneral].

  Therefore we simply wrap these functions by correct_levels.
  They may be slower but I do not want to be bothered by strange
  type level bugs.
*)
  include Ctype
    
  let expand_head env ty = expand_head env & correct_levels ty
  let apply env tys ty tys2 =
    apply env (map correct_levels tys) (correct_levels ty) (map correct_levels tys2)
  let moregeneral env b ty1 ty2 = moregeneral env b (correct_levels ty1) (correct_levels ty2)
end

module Types = struct
  include Types
  open Btype
  open Ctype
  let repr_desc ty = (repr ty).desc

  let expand_repr_desc env ty = (repr & expand_head env ty).desc

  let with_snapshot f =
    let snapshot = snapshot () in
    let res = protect f in
    backtrack snapshot;
    unprotect res

  let is_constr env ty = match expand_repr_desc env ty with
    | Tconstr (p, tys, _) -> Some (p, tys)
    | _ -> None
  
  let is_option_type env ty = match is_constr env ty with
    | Some (po, [ty]) when po = Predef.path_option -> Some ty
    | _ -> None

  let gen_vars ty =
    flip filter (Ctype.free_variables ty) & fun ty ->
      ty.level = Btype.generic_level

  (* Create a type which can be unified only with itself *)
  let create_uniq_type =
    let cntr = ref 0 in
    fun () -> 
      incr cntr;
      (* Ident.create is not good. Unifying this data type ident with
         a tvar may cause "escaping the scope" errors
      *)
      Ctype.newty ( Tconstr ( Path.Pident (Ident.create_persistent & "*uniq*" ^ string_of_int !cntr), [], ref Mnil ) )

  let close_gen_vars ty = flip iter (gen_vars ty) & fun gv ->
    match repr_desc gv with
    | Tvar _ ->
        Ctype.unify Env.empty gv (create_uniq_type ());
       (* eprintf "Closing %a@." Printtyp.type_expr gv *)
    | Tunivar _ -> ()
    | _ -> assert false
end


open List

open Longident
open Path
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
  
(** Build an empty type env except mp module with the given module type *)
class dummy_module env mp mty =
  (* Env.lookup_* does not support Mty_alias (and probably Mty_indent) *)
  let mty = Env.scrape_alias env & Mtype.scrape env mty in
(*
  let () = !!% "dummy_module of @[%a@]@." Printtyp.modtype mty in 
*)
  let dummy = "Dummy" in
  let id = Ident.create "Dummy" in
  let env = Env.add_module id mty Env.empty in
object

  method lookup_type s =
    match Env.lookup_type (Longident.(Ldot (Lident dummy, s))) env with
    | Pdot (_, s', n) as p ->
       let td = Env.find_type p env in
       Pdot (mp, s', n), td
    | _ -> assert false (* impos *)

  method lookup_module s =
    match Env.lookup_module ~load:false (Longident.(Ldot (Lident dummy, s))) env with
    | Pdot (_, s', n) -> Pdot (mp, s', n)
    | _ -> assert false (* impos *)
    
  method lookup_value s =
    match Env.lookup_value (Longident.(Ldot (Lident dummy, s))) env with
    | Pdot (_, s', n), _vd -> Pdot (mp, s', n)
    | _ -> assert false (* impos *)
    
end

let mangle s = 
  let len = String.length s in
  let b = Buffer.create len in
  for i = 0 to len - 1 do
    let c = String.unsafe_get s i in
    match c with
    | 'A'..'Z' | 'a'..'z' | '0'..'9' | '\'' -> Buffer.add_char b c
    | '_' -> Buffer.add_string b "__"
    | _ -> 
        Buffer.add_char b '_';
        Buffer.add_string b & Printf.sprintf "%02x" & Char.code c
  done;
  Buffer.contents b

(* CR jfuruse: need tests *)
let unmangle s = 
  try
    let len = String.length s in
    let b = Buffer.create len in
    let rec f i = 
      if i = len then ()
      else begin
        let c = String.unsafe_get s i in
        match c with
        | 'A'..'Z' | 'a'..'z' | '0'..'9' | '\'' -> Buffer.add_char b c; f & i+1
        | '_' -> 
            begin match s.[i+1] with
            | '_' -> Buffer.add_char b '_'; f & i+2
            | _ ->
                let hex = String.sub s (i+1) 2 in
                let c = Char.chr & int_of_string & "0x" ^ hex in
                Buffer.add_char b c;
                f & i+3
            end
        | _ -> raise Exit
      end
    in
    f 0;
    Ok (Buffer.contents b)
  with
  | Failure e -> Error (`Failed_unmangle (s ^ ": " ^ e))

let expression_from_string s = 
  let lexbuf = Lexing.from_string s in
  try Ok (Parser.parse_expression Lexer.token lexbuf) with
  | _ -> Error (`Parse s)

let tvars_of_core_type cty =
  (* Using mapper for iterator is redundant, but easiest way *)
  let open Ast_mapper in
  let open Parsetree in
  let vars = ref [] in
  let extend super =
    let typ self ty =
      match ty.ptyp_desc with
      | Ptyp_var s ->
          if not & mem s !vars then vars := s :: !vars;
          ty
      | _ -> super.typ self ty
    in
    { super with typ }
  in
  let m = extend default_mapper in
  ignore & m.typ m cty;
  !vars

let sig_module_of_stri sitem =
  let open Parsetree in
  match sitem.pstr_desc with
  | Pstr_modtype mtd ->
      Ast_helper.Sig.module_ ~loc:sitem.pstr_loc
        { pmd_name       = mtd.pmtd_name
        ; pmd_type       = from_Some mtd.pmtd_type
        ; pmd_attributes = mtd.pmtd_attributes
        ; pmd_loc        = mtd.pmtd_loc
        }
  | _ ->
      !!% "sig_module_of_stri: got a non modtype@.";
      assert false


let rec values_of_module ~recursive env path mdecl : Path.t list =
  let m = new dummy_module env path mdecl.md_type in
  let sg = scrape_sg env mdecl in
  flip2 fold_right sg [] & fun sitem st -> match sitem with
    | Sig_value (id, _vdesc) ->
        let path = try m#lookup_value & Ident.name id with Not_found ->
          !!% "values_of_module: m#lookup_value %s not found@." & Ident.name id;
          assert false
        in
        path :: st
    | Sig_module (id, moddecl, _) when recursive -> 
        let path = m#lookup_module & Ident.name id in
        values_of_module ~recursive env path moddecl @ st
          
    | _ -> st

let check_module env loc path =
  match 
    try Some (Env.find_module path env) with _ -> None
  with
  | None -> 
      raise_errorf "%a: no module desc found: %a" Location.format loc Path.format path
  | Some mdecl -> mdecl

let values_of_module ~recursive env loc path =
  let mdecl = check_module env loc path in
  values_of_module ~recursive env path mdecl

let format_expression ppf e =
  Pprintast.expression ppf
  & (* Typpx. *) Untypeast.(default_mapper.expr default_mapper) e

let is_none e =
  let open Typedtree in
  match e.Typedtree.exp_desc with
  | Texp_construct ({Location.txt=Lident "None"}, _, []) -> 
      begin match is_option_type e.exp_env e.exp_type with
      | None ->
          !!% "is_none: the input is type-corrupted@."; assert false
      | Some ty -> Some ty
      end
  | _ -> None


      

(*
(** Typing tools *)
    
val scrape_sg : Env.t -> Types.module_declaration -> Types.signature

val fold_module
  : Env.t
  -> Path.t
  -> 'a ->
  ('a
   -> [> `Class   of Ident.t * Path.t * Types.class_declaration
       | `Cltype  of Ident.t * Path.t * Types.class_type_declaration
       | `Modtype of Ident.t * Path.t * Types.modtype_declaration
       | `Module  of Ident.t * Path.t * Types.module_declaration
       | `Type    of Ident.t * Path.t * Types.type_declaration
       | `Typext  of Ident.t * Path.t * Types.extension_constructor
       | `Value   of Ident.t * Path.t * Types.value_description ]
   -> 'a)
  -> 'a

*)
    
module Forge = struct
  open Typedtree

  let default_loc = ref Location.none

  (* let loc txt = { Location.loc= !default_loc; txt } *)
  (* let lidentloc_of_path p = loc @@ Untypeast.lident_of_path p *)

  module Dummy = struct
  
    open Types
    open Typedtree

    let type_expr = Btype.newgenty (Types.Tvar None)

    let env = Env.empty
  
    let value_description () = 
      { val_type       = type_expr;
        val_kind       = Val_reg;
        val_loc        = !default_loc;
        val_attributes = [] 
      }
        
    let exp_desc = Texp_tuple []
  
    let exp () = 
      { exp_desc;
        exp_loc        = !default_loc;
        exp_extra      = [];
        exp_type       = type_expr;
        exp_env        = env;
        exp_attributes = [] 
      }
  
    let mod_type = Mty_signature [] 
  
    let structure_item () = 
      { str_desc = Tstr_recmodule []
      ; str_loc = !default_loc
      ; str_env = env 
      }
  end

  let loc txt = { Location.loc= !default_loc; txt }
  
  let lidentloc_of_path p = loc @@ Untypeast.lident_of_path p
  
  let with_loc loc f = 
    let back = !default_loc in
    default_loc := loc;
    let res = f () in
    default_loc := back;
    res
  
  module Path = struct
    open Longident
    open Path
    type t = Path.t
    let rec of_lident = function
      | Lident s -> Pident (Ident.create s)
      | Ldot (t,s) -> Pdot (of_lident t, s, 0)
      | Lapply (t1,t2) -> Papply (of_lident t1, of_lident t2)
  end
  
  module Typ = struct
    open Types
    open Ctype
    let arrow ?(label=Nolabel) t1 t2 = newty (Tarrow (label, t1, t2, Cunknown)) 
  end
  
  module Exp = struct
  
    (* directly embed Parsetree.expression *)    
    let untyped pexp = 
      { (Dummy.exp ()) 
        with exp_attributes= [ {txt="typpx_embed"; loc= !default_loc}, 
                               Parsetree.PStr ([ Ast_helper.Str.eval pexp ]) ] }
  
    let ident p = 
      { (Dummy.exp ()) with
        exp_desc = Texp_ident (p, lidentloc_of_path p, Dummy.value_description ()) } 
  
    let let_ ?(recursive=false) vbs e =
      { (Dummy.exp ()) with
        exp_desc = Texp_let((if recursive then Recursive else Nonrecursive),
                            vbs,
                            e)
      }
  
    let letmodule id mexpr e =
      { (Dummy.exp ()) with 
        exp_desc = Texp_letmodule (id, loc (Ident.name id), mexpr, e) }
  
    let app e les =
      match les with
      | [] -> e
      | _ ->
          { (Dummy.exp ()) with
            exp_desc = Texp_apply(e, map (fun (l,e) -> l, Some e) les)
          }
  
    let ignore x =
      let lident_ignore = Longident.(Ldot ( Lident "Pervasives", "ignore" )) in
      let path_ignore = Path.of_lident lident_ignore in
      let ignore = ident path_ignore in
      { x with exp_desc = Texp_apply (ignore, [Nolabel, Some x]) }
  
    let fun_ ?(label=Nolabel) c_lhs e =
      let cases = [ {c_lhs; c_guard=None; c_rhs= e} ] in
      let param = Typecore.name_pattern "param" cases in
      { e with exp_desc = Texp_function { arg_label= label
                                        ; param
                                        ; cases
                                        ; partial= Total 
                                        }
      }
  
    let tuple es = { (Dummy.exp ()) with exp_desc = Texp_tuple es }
  
    let with_env ev e = { e with exp_env = ev }
  
    let check_constructor_is_for_path ev s path =
      let lid = Longident.Lident s in
      try
        let cdesc = Env.lookup_constructor lid ev in
        match (Ctype.repr cdesc.cstr_res).desc with
        | Tconstr (p, _, _) when p = path -> ()
        | _ -> 
            Format.eprintf "Typpx.Forge.Exp: %a is not accessible in this scope@." Longident.format lid;
            assert false
      with Not_found ->
        Format.eprintf "Typpx.Forge.Exp: %a is not accessible in this scope@." Longident.format lid;
        assert false
      
    let none ?(ty=Dummy.type_expr) ev =
      check_constructor_is_for_path ev "None" Predef.path_option; 
      Typecore.option_none ty !default_loc
        
    let some ev e =
      check_constructor_is_for_path ev "Some" Predef.path_option; 
      Typecore.option_some e
  
    let list ev es =
      check_constructor_is_for_path ev "::" Predef.path_list;
      check_constructor_is_for_path ev "[]" Predef.path_list;
      let ty = match es with
        | [] -> Dummy.type_expr
        | e::_ -> e.exp_type
      in
      let cnull = Env.lookup_constructor (Longident.Lident "[]") ev in
      let ccons = Env.lookup_constructor (Longident.Lident "::") ev in
      let null ty = 
        { (Dummy.exp ()) with
          exp_desc = Texp_construct ( loc (Longident.Lident "[]"),
                                      cnull, [] );
          exp_type = Predef.type_list ty
        }
      in
      let cons e e' =
        { (Dummy.exp ()) with
          exp_desc = Texp_construct ( loc (Longident.Lident "::"),
                                      ccons, [e; e'] ) }
      in
      fold_right (fun e st ->
        { (cons e st) with exp_type = Predef.type_list e.exp_type })
        es (null ty)
        
    open Asttypes
  
    let mark txt e =
      { e with exp_attributes= ({txt; loc= Ast_helper.ghost e.exp_loc}, Parsetree.PStr []) 
                               :: e.exp_attributes }
        
    let partition_marks e f =
      let g = function
        | {txt}, Parsetree.PStr [] when f txt -> `Left txt
        | a -> `Right a
      in
      let marks, exp_attributes = partition_map g e.exp_attributes in
      marks,
      { e with exp_attributes }
  
  end
  
  module Pat = struct
  
    let desc d = 
      { pat_desc = d;
        pat_loc = !default_loc;
        pat_extra = [];
        pat_type = Dummy.type_expr;
        pat_env = Dummy.env;
        pat_attributes = [];
      }
  
    let var id = desc (Tpat_var (id, loc (Ident.name id)))
  end
  
  module MB = struct
    let module_binding id x = { mb_id = id
                              ; mb_name = loc id.Ident.name
                              ; mb_expr = x
                              ; mb_attributes = []
                              ; mb_loc = !default_loc 
                              } 
  end
  
  module Mod = struct
    let of_module_expr_desc d = 
      { mod_desc = d;
        mod_loc = !default_loc;
        mod_type = Dummy.mod_type;
        mod_env = Dummy.env;
        mod_attributes = [] }
  
    let ident p = of_module_expr_desc @@ Tmod_ident (p, lidentloc_of_path p)
  
    let unpack e = of_module_expr_desc @@ Tmod_unpack (e, Dummy.mod_type)
  end
end

let at ?loc txt = 
  let loc = match loc with 
    | None -> !Ast_helper.default_loc
    | Some loc -> loc
  in
  { txt; loc }

let (!@) x = at x
