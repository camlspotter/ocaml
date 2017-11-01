open Leopardutils
open Leopardcomplib

open Asttypes

module Debug = struct
  let env_exist s = try ignore (Sys.getenv s); true with _ -> false
  let debug_resolve = env_exist "LEOPARD_IMPLICITS_DEBUG_RESOLVE"
  let debug_unif = env_exist "LEOPARD_IMPLICITS_DEBUG_UNIF"
end

module Klabel = struct
  (** Constraint labels *)
    
  open Types
  open List

  let is_klabel = function
    | Labelled s when s.[0] = '_' -> Some `Normal
    | Optional s when s.[0] = '_' -> Some `Optional
    | _ -> None
  
  (* Constraint labels must precede the other arguments *)
  let rec extract env ty = 
    let ty = Ctype.expand_head env ty in
    match repr_desc ty with
    | Tarrow(l, ty1, ty2, _) when is_klabel l <> None ->
        let cs, ty = extract env ty2 in
        (l,ty1)::cs, ty
  (*
    | Tarrow(l, ty1, ty2, x) ->
        let cs, ty = extract env ty2 in
        cs, { (Ctype.newty & Tarrow (l, ty1, ty, x)) with level = ty.level }
  *)
    | _ -> [], ty
  
  let rec extract_aggressively env ty =
    let ty = Ctype.expand_head env ty in
    match repr_desc ty with
    | Tarrow(l, ty1, ty2, _) when gen_vars ty1 <> [] ->
        ([], ty)
        :: map
          (fun (cs, ty) -> (l,ty1)::cs, ty)
          (extract_aggressively env ty2)
    | _ -> [[], ty]
  
  (*
  let rec get_args ty =
    let ty = Ctype.expand_head env ty in
    match repr_desc ty with
    | Tarrow(l, ty1, ty2, _) -> (l,ty1)::get_args ty2
    | _ -> []
  *)
  
end

module Candidate = struct
  (*
  
    Instance search space specification DSL, mangling to and back from
    OCaml type definitions.
  
  *)
  open List
  open Path
  open Types
  
  type t = {
    path       : Path.t;
    expr       : Typedtree.expression;
    type_      : Types.type_expr;
    aggressive : bool
  }
  
  let format ppf c =
    Format.fprintf ppf "@[<2>\"%a\" : %a@ : %a@]"
      Path.format c.path
      format_expression c.expr
      Printtyp.type_scheme c.type_
      
  let uniq xs =
    let tbl = Hashtbl.create 107 in
    iter (fun x ->
      try
        let x' = Hashtbl.find tbl x.path in
        Hashtbl.replace tbl x.path { x with aggressive = x.aggressive || x'.aggressive }
      with
      | Not_found -> Hashtbl.add tbl x.path x) xs;
    Hashtbl.to_list tbl |> map snd
  
  let get_opens env =
    let open Env in (* We need this because of -principal *)
    let rec get = function
      | Env_empty -> []
      | Env_value (s, _, _) 
      | Env_type (s, _, _)
      | Env_extension (s, _, _)
      | Env_module (s, _, _)
      | Env_modtype (s, _, _)
      | Env_class (s, _, _)
      | Env_cltype (s, _, _)
      | Env_constraints (s, _)
      | Env_functor_arg (s, _) -> get s
      | Env_open (s, path) -> path :: get s
    in
    get & Env.summary env
  
  let _dump_summary env =
    let open Env in
    let rec dump = function
      | Env_empty -> ()
      | Env_value (s, _, _)
      | Env_extension (s, _, _)
      | Env_modtype (s, _, _)
      | Env_class (s, _, _)
      | Env_cltype (s, _, _)
      | Env_constraints (s, _)
      | Env_functor_arg (s, _) -> dump s
      | Env_type (s, id, _) -> !!% "type %a@." Ident.format id; dump s
      | Env_module (s, id, _) -> !!% "module %a@." Ident.format id; dump s
      | Env_open (s, path) -> !!% "open %a@." Path.format path; dump s
    in
    dump & Env.summary env
    
  let module_lids_in_open_path env lids = function
    | None -> 
        (* Finds lids in the current scope, but only defined ones in the current scope level.
             * Persistent ones are excluded
             * Sub-modules are excluded
        *)
        flip filter_map lids (fun lid ->
          try
            let p = Env.lookup_module ~load:true (*?*) lid env in
            match p with
            | Pident id when not & Ident.persistent id -> Some p
            | _ -> None (* not sure... *)
          with
          | _ -> None)
    | Some open_ ->
        (*  !!% "open %a@." Path.format open_; *)
        let mdecl = Env.find_module open_ env in (* It should succeed *)
        let sg = scrape_sg env mdecl in
        let env = Env.open_signature Fresh open_ sg Env.empty in
        flip filter_map lids (fun lid ->
          try
            Some (Env.lookup_module ~load:true (*?*) lid env)
          with
          | _ -> None)

  (* Create a typed expression of the given path in the env *)
  (* XXX moved to Leopardcomplib *)
  let expr_of_path ?(loc=Location.none) env path =
    let open Typedtree in
    let vdesc = Env.find_value path env in
    let type_ = vdesc.val_type in
    { exp_desc= Texp_ident (path
                           , { txt= Untypeast.lident_of_path path
                             ; loc }
                           , vdesc)
    ; exp_loc = loc
    ; exp_extra = []
    ; exp_type = Ctype.instance env type_
    ; exp_env = env
    ; exp_attributes = []
    }
    
  let default_candidate_of_path env path = 
    try
      let vdesc = Env.find_value path env in
      let type_ = vdesc.val_type in
      let expr = expr_of_path env path in
      { path; expr; type_; aggressive= false }
    with
    | Not_found -> assert false (* impos *)
  
  let cand_direct env loc (flg,lid,popt) =
    let recursive = match flg with
      | `Just -> false
      | `In -> true
    in
    let path = match popt with
      | Some p -> p
      | None -> 
          try
            Env.lookup_module ~load:true lid env
          with
          | Not_found -> raise_errorf "%a: Unbound module %a." Location.format loc Longident.format lid
    in
    let paths = values_of_module ~recursive env loc path in
    map (default_candidate_of_path env) paths
  
  let cand_opened env loc (flg,lid) =
    let opens = get_opens env in
    if Debug.debug_resolve then begin
      !!% "debug_resolve: cand_opened opened paths@.";
      flip iter opens & !!% "  %a@." Path.format
    end;
    let paths = 
      concat 
      & map (module_lids_in_open_path env [lid]) 
      & None :: map (fun x -> Some x) opens
    in
    if Debug.debug_resolve then begin
      !!% "debug_resolve: cand_opened cand modules@.";
      flip iter paths & !!% "  %a@." Path.format
    end;
    concat & map (fun path ->
      let lid = Untypeast.lident_of_path path in
      cand_direct env loc
        & match flg with
        | `Just -> `Just, lid, Some path
        | `In -> `In, lid, Some path) paths
  
  
(*
  let cand_name rex f =
    f () |> filter (fun x -> Re_pcre.pmatch ~rex & Longident.to_string & (* Typpx. *) Untypeast.lident_of_path x.path)
*)
end

module Crelated = struct end
module Chastype = struct end
module Cderiving = struct end
module Cppxderive = struct end

module Spec = struct
(*

  Instance search space specification DSL, magling to and back from
  OCaml type definitions.

*)
open List

open Parsetree
open Types

(** spec dsl *)
type t = t2 list (** [t2, .., t2] *)

and t2 = 
  | Opened of [`In | `Just] * Longident.t (** [opened M]. The values defined under module path [P.M] which is accessible as [M] by [open P] *)
  | Direct of [`In | `Just] * Longident.t * Path.t option
    (** [P] or [just P]. [P] is for the values defined under module [P] and [P]'s sub-modules. [just P] is for values defined just under module [P] and values defined in its sub-modules are not considered. *)
  | Aggressive of t2 (** [aggressive t2]. Even normal function arrows are considered as constraints. *)
  | Related (** [related]. The values defined under module [P] where data type defined in [P] appears in the type of the resolution target *)
(*
  | Name of string * Re.re * t2 (** [name "rex" t2]. Constraint values only to those whose names match with the regular expression *)
*)
  | Has_type of core_type * type_expr option (** [typeclass path]. Typeclass style resolution.  *) 
  | Deriving of Longident.t (** [deriving M]. [M] must define [M.tuple], [M.object_] and [M.poly_variant] *)
  | PPXDerive of Parsetree.expression * core_type * type_expr option (** [ppxderive ([%...] : ty)]. *)
      
let rec is_static = function
  | Opened _ -> true
  | Direct _ -> true
  | Related -> false
  | Aggressive t2 (* | Name (_, _, t2) *) -> is_static t2
  | Has_type _ -> true
  | Deriving _ -> false
  | PPXDerive _ -> false
    
let to_string = 
  let rec t = function
    | [x] -> t2 x
    | xs -> String.concat ", " (map t2 xs)
  and t2 = function
    | Direct (`In, l, _) -> Longident.to_string l
    | Direct (`Just, l, _) -> Printf.sprintf "just %s" & Longident.to_string l
    | Opened (`In, l) -> Printf.sprintf "opened (%s)" & Longident.to_string l
    | Opened (`Just, l) -> Printf.sprintf "opened (just %s)" & Longident.to_string l
    | Related -> "related"
    | Aggressive x -> Printf.sprintf "aggressive (%s)" (t2 x)
(*
    | Name (s, _re, x) -> Printf.sprintf "name %S (%s)" s (t2 x)
*)
    | Has_type (cty, _) -> 
        Format.asprintf "has_type (%a)"
          Pprintast.core_type cty
    | Deriving p -> Printf.sprintf "deriving %s" & Longident.to_string p
    | PPXDerive (e, cty, _) ->
        Format.asprintf "ppxderive (%a : %a)"
          Pprintast.expression e
          Pprintast.core_type cty
  in
  t 

(** spec to candidates *)

open Candidate
    
let rec cand_static env loc : t2 -> t list = function
  | Aggressive x ->
      map (fun x -> { x with aggressive = true }) & cand_static env loc x
  | Opened (f,x) -> cand_opened env loc (f,x)
  | Direct (f,x,popt) -> cand_direct env loc (f,x,popt)
(*
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_static env loc t2
*)
(*
  | Has_type (_, Some ty) -> Chas_type.cand_has_type env loc ty
*)
  | Has_type _ -> assert false (* impos *)
  | spec when is_static spec -> assert false (* impos *)
  | _ -> assert false (* impos *)

let rec cand_dynamic env loc ty = function
(*
  | Related -> Crelated.cand_related env loc ty
*)
  | Aggressive x -> map (fun x -> { x with aggressive= true }) & cand_dynamic env loc ty x
(*
  | Name (_, rex, t2) -> cand_name rex & fun () -> cand_dynamic env loc ty t2
*)
(*
  | Deriving lid -> Cderiving.cand_deriving env loc ty lid
  | PPXDerive (_e, _cty, None) -> assert false (* impos *)
  | PPXDerive (e, _cty, Some temp_ty) -> Cppxderive.cand_derive env loc e temp_ty ty 
*)
  | Opened _ | Direct _ | Has_type _ ->
      (* they are static *)
      assert false (* impos *)
  | _ -> assert false

let candidates env loc = function
  | ts ->
      let statics, dynamics = partition is_static ts in
      let statics = concat & map (cand_static env loc) statics in
      if Debug.debug_resolve then begin
        !!% "debug_resolve: static candidates@.";
        flip iter statics & fun x ->
          !!% "  %a@." Pprintast.expression (Untypeast.(default_mapper.expr default_mapper) x.expr)
      end;
      let dynamics ty = concat & map (cand_dynamic env loc ty) dynamics in
      fun ty -> uniq & statics @ dynamics ty
    
end

module Specconv = struct
  open List
  
  open Parsetree
  open Types
  
  open Spec
  
  (* Spec conversion 
  
     * between attribute expression: [%imp <e>] <=> spec
     * between type definition: type __imp_spec__ <=> [%%imp_spec <e>] <=> spec
  *)
  
  let prefix = "Spec_"
  let prefix_len = String.length prefix
  
  (** Extract type parts so that they can be encoded in the parameter part
      of a variant constructor.
  *)
  let get_type_components = 
    let rec t = function
      | xs -> concat_map t2 xs
    and t2 = function
      | Direct (`In, _l, _) -> []
      | Direct (`Just, _l, _) -> []
      | Opened (`In, _l) -> []
      | Opened (`Just, _l) -> []
      | Related -> []
      | Aggressive x -> t2 x
  (*
      | Name (_s, _re, x) -> t2 x
  *)
      | Has_type (cty, _ty) -> [cty]
      | Deriving _p -> []
      | PPXDerive (_e, cty, _) -> [cty]
    in
    t 
  
  (** Assign types of variant constructor parameter to spec *)
  let assign_type_components tys t0 = 
    let rec t tys = function
      | xs ->
          let tys, rev_xs = 
            fold_left (fun (tys,rev_xs) x ->
              let tys, x = t2 tys x in
              tys, x :: rev_xs) (tys,[]) xs
          in
          tys, (rev rev_xs)
            
    and t2 tys x = match x with
      | Direct _ -> tys, x
      | Opened _ -> tys, x
      | Related -> tys, x
      | Aggressive x ->
          let tys, x = t2 tys x in
          tys, Aggressive x
  (*
      | Name (s, re, x) ->
          let tys, x = t2 tys x in
          tys, Name (s, re, x)
  *)
      | Has_type (cty, None) ->
          begin match tys with
          | ty::tys -> tys, Has_type (cty, Some ty)
          | _ -> assert false (* impos *)
          end
      | Has_type _ -> assert false (* impos *)
      | Deriving _ -> tys, x
      | PPXDerive (e, cty, None) ->
          begin match tys with
          | ty::tys -> tys, PPXDerive (e, cty, Some ty)
          | _ -> assert false (* impos *)
          end
      | PPXDerive (_e, _cty, Some _) -> assert false (* impos *)
    in
    match t tys t0 with
    | [], t0 -> t0
    | _ -> assert false (* impos *)
  
  let mangle x =
    (prefix ^ mangle (to_string x),
     get_type_components x)
  
  (* CR jfuruse: need tests *)
  let unmangle_spec_string s = 
    if not & String.is_prefix prefix s then raise_errorf "Mangled spec string does not start with \"Spec_\": %s" s;
    let s = String.sub s prefix_len (String.length s - prefix_len) in
    unmangle s
  
  let from_string = expression_from_string
  
  let from_expression _env e =
    let open Longident in
    try
      let get_lid e = match e.pexp_desc with
        | Pexp_construct ({txt=lid}, None) -> Some lid
        | _ -> None
      in
      let rec t e = match e.pexp_desc with
        | Pexp_tuple xs -> map t2 xs
        | _ -> [t2 e]
      and t2 e = match e.pexp_desc with
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "aggressive"} },
                      [Nolabel, e] ) -> Aggressive (t2 e)
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "opened"} },
                      [Nolabel, e] ) ->
            let f,l = flag_lid e in Opened (f,l)
  (*
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "name"} },
                      [ Nolabel, { pexp_desc = Pexp_constant (Pconst_string (s, _)) }
                      ; Nolabel, e ] ) -> Name (s, Re_pcre.regexp s, t2 e)
  *)
        | Pexp_ident {txt=Lident "related"} -> Related
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "has_type"} }, args ) ->
             begin match args with
            | [Nolabel, e] ->
                begin match e.pexp_desc with
                | Pexp_ident lid ->
                    Has_type (Ast_helper.Typ.constr lid [], None)
                | _ -> raise_errorf "has_type must take a type path"
                end
            | _ -> raise_errorf "has_type must take just one argument"
            end
            
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "deriving"} }, args ) ->
            begin match args with
            | [Nolabel, e] -> 
                begin match get_lid e with
                | Some lid -> Deriving lid
                | None -> raise_errorf "deriving must take an module path" Location.format e.pexp_loc
                end
            | _ -> raise_errorf "deriving must take just one argument"
            end
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "ppxderive"} }, args ) ->
            begin match args with
            | [Nolabel, e] ->
                begin match e.pexp_desc with
                | Pexp_constraint (e, cty) -> PPXDerive (e, cty, None)
                | _ -> raise_errorf "ppxderive must take (e : t)"
                end
            | _ -> raise_errorf "ppxderive must take just one argument"
            end
        | _ -> 
            let f,lid = flag_lid e in
            Direct (f, lid, None)
      and flag_lid e = match e.pexp_desc with
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "just"} },
                      [Nolabel, e] ) -> 
            begin match get_lid e with
            | Some lid -> `Just, lid
            | None -> raise_errorf "%a: just requires an argument" Location.format e.pexp_loc
            end
        | Pexp_construct ({txt=lid}, None) -> `In, lid
        | _ ->
            raise_errorf "%a: Illegal spec expression: %a" 
              Location.format e.pexp_loc
              Pprintast.expression e
              
      in
      Ok (t e)
    with
    | Failure s -> Error (`ParseExp (e, s))
  
  let from_structure env str =
    match str with
    | [] -> Error (`String "requires implicit policies")
    | _::_::_ -> 
        Error (`String "multiple implicit policies are not allowed")
    | [sitem] ->
        match sitem.pstr_desc with
        | Pstr_eval (e, _) ->
            from_expression env e
        | _ ->
            Error (`String "spec must be an OCaml expression")
  
  let error loc = function
    | `String s -> raise_errorf "%a: %s" Location.format loc s
    | `Failed_unmangle s -> 
        raise_errorf "%a: Illegal spec encoding: %S" Location.format loc s
    | `Parse s ->
        raise_errorf "%a: Spec parse failed: %S" Location.format loc s
    | `ParseExp (_, s) ->
        raise_errorf "%a: Spec parse failed: %s" Location.format loc s
  
  let from_payload_ env = function
    | PStr s -> from_structure env s
    | _ -> Error (`String "spec must be an OCaml expression")
  
  let from_payload loc x = match from_payload_ Env.empty (* dummy *) x with
    | Error err -> error loc err
    | Ok spec -> spec
  
  (* typed world *)
  
  (********************* NEW ENCODING USING POLYVARIANT ***************)
      
  (** Obtain a spec from a type expr. *)
  let from_type_expr env loc ty = match expand_repr_desc env ty with
    | Tvariant rd ->
        let rd = Btype.row_repr rd in
        begin match rd with
        | { row_closed= true; row_fields= [l, Rpresent (Some ty)] } ->
            let open Result.Monad in
            (* Note that the type variables are Tunivars *)
            let unpoly ty = match expand_repr_desc env ty with
              | Tpoly (ty, []) -> ty
              | _ -> ty
            in
            let fs, nil = Ctype.(flatten_fields & object_fields ty) in
            let fs = map (fun (a,b,ty) -> (a,b,unpoly ty)) fs in
            assert (expand_repr_desc env nil = Tnil);
            assert (fs = []
                   || for_all (function (_, Fpresent, _) -> true | _ -> false) fs);
            assign_type_components (map (fun (_,_,ty) -> ty) fs)
            & at_Error (error loc)
            & unmangle_spec_string l
              >>= from_string
              >>= from_expression env
        | _ -> 
            raise_errorf "%a: Illegal type for implicit spec" Location.format loc
        end
    | _ -> 
        raise_errorf "%a: Illegal type for implicit spec" Location.format loc
  
  let to_core_type _loc spec = (* XXX we should use loc *)
    let open Ast_helper in
    let mangled, ctys = mangle spec in
    let label n = !@ ("l" ^ string_of_int n) in
    let make_meth_type cty =  (* quantify cty *)
      Typ.poly (List.map (!@) & tvars_of_core_type cty) cty
    in
    let oty = Typ.object_ (mapi (fun n cty -> (label n, [], make_meth_type cty)) ctys) Closed
    in
    Typ.variant [ Parsetree.Rtag (mangled, [], false, [oty]) ] Closed None

  let from_payload_to_core_type : Location.t -> payload -> core_type = fun loc x -> 
    to_core_type loc & from_payload loc x

  let () = Leopardppx.Imp.from_payload_to_core_type_forward := from_payload_to_core_type
  
end

module Pre_typeclass = struct
(*

  Pre-preprocessing for syntax sugars for 
    [@@typeclass]
    [@@instance]
*)

open Ast_helper

open Parsetree
open Ast_mapper
open List

module VarInInstance : sig
  val replace_var_extensions : core_type -> core_type * string list
end = struct
  open Longident
    
  let vars = ref []
    
  let extend super =
    let typ self cty = match cty.ptyp_desc with
      | Ptyp_extension ({txt="var"; loc}, PTyp cty) ->
          begin match cty.ptyp_desc with
          | Ptyp_var n ->
              let n = "__var_" ^ n in
              vars := n :: !vars;
              Typ.constr ~loc:cty.ptyp_loc (at ~loc:cty.ptyp_loc & Lident n) []
          | _ -> 
              raise_errorf "%a: Illegal %%var.  The correct syntax is: [%%var:'a]"
                Location.format loc
          end
      | Ptyp_extension ({txt="var"; loc}, _) ->
          raise_errorf "%a: Illegal %%var.  The correct syntax is: [%%var:'a]"
            Location.format loc
      | _ -> super.typ self cty
    in
    { super with typ }

  let mapper = extend default_mapper
    
  let replace_var_extensions cty =
    vars := [];
    let cty = mapper.typ mapper cty in
    cty, sort_uniq compare !vars
end

(***

(* [@@typeclass] and [@@instance] *)
module TypeClass : sig
  val gen_declaration
    : module_type_declaration
    -> structure_item
    
  val parse_instance_declaration
    : Location.t
    -> payload
    -> Longident.t * Location.t * (string * core_type) list * string list

  val gen_instance
    : Longident.t
    -> module_binding
    -> instance_loc: Location.t
    -> (string * core_type) list
    -> string list
    -> structure_item    
end = struct
  open Longident

  (* module type Show = sig 
     type a 
     type b
     val show : a -> b -> string 
     end
     => [a; b]
  *)
  let parameters sg = sort compare & concat_map (fun si ->
    match si.psig_desc with
    | Psig_type (_rf, tds) ->
        flip filter_map tds & fun td ->
          begin match td with
          | { ptype_name = {txt}; ptype_params = []; ptype_cstrs = []; ptype_kind = Ptype_abstract; ptype_manifest = None; } -> Some txt
          | _ -> None
          end
    | _ -> []) sg

  (* module type Show = sig 
       type a 
       type b
       val show : a -> b -> string 
     end
     => ["show", a -> b -> string]
  *)
  let values = filter_map & fun si ->
    match si.psig_desc with
      | Psig_value vdesc -> Some (vdesc.pval_name.txt, vdesc.pval_type)
      | _ -> None

  let add_newtypes = fold_right (fun s -> Exp.newtype ?loc:None s)

  let link = [%stri type __class__ ]

  (* type ('a, 'b) _module = (module Show with type a = 'a and b = 'b) *)
  let gen_ty_module name ps =
    let tvars = map (Typ.var ?loc:None) ps in (* CR jfuruse: loc *)
    Type.mk ?loc:None
      ~params: (map (fun tv -> (tv, Invariant)) tvars)
      ~manifest: (Typ.package ?loc:None 
                   (at (Lident name)) 
                   (map2 (fun p tv -> (at (Lident p), tv)) ps tvars))
      (at "_module") (* CR jfuruse: can have a ghost loc *)

  (* type ('a, 'b) _class = (('a, 'b) _module, [%imp has_type __class__]) Ppx_implicits.t *)
  let gen_ty_class ps =
    let tvars = map (Typ.var ?loc:None) ps in (* CR jfuruse: loc *)
    Type.mk ?loc:None
      ~params: (map (fun tv -> (tv, Invariant)) tvars)
      ~manifest: [%type: ([%t Typ.constr (at (Lident "_module")) tvars], [%imp has_type __class__]) Ppx_implicits.t]
      (at "_class") (* CR jfuruse: can have a ghost loc *)
  ;;

  (* let show (type a) ?_d:(_d : a _class option) =
    let module M = (val (Ppx_implicits.(get (from_Some _d)))) in
    M.show
  *)

  let method_ tys (n,_ty) = 
    let paramed_s = 
      let open Typ in
      constr (at ?loc:None & Lident "_class") 
      & map (fun ty -> constr ?loc:None (at ?loc:None & Lident ty) []) tys
    in
    [%stri let [%p Pat.var' ?loc:None n] =
        [%e add_newtypes (List.map (!@) tys)
            [%expr fun ?_d:(_d : [%t paramed_s] option) -> 
                     let module M = (val (Ppx_implicits.(get (from_Some _d)))) in
                     [%e Exp.(ident ?loc:None (at ?loc:None (Ldot (Lident "M", n)))) ] ] ] ]

  let gen_declaration mtd =
    let name = mtd.pmtd_name.txt in
    match mtd.pmtd_type with 
    | Some { pmty_desc = Pmty_signature sg } -> 
        let ps = parameters sg in
        let vs = values sg in
        begin match ps with
        | [] ->
            raise_errorf "%a: sig .. end [@@@@typeclass] requires at least one parameter type declaration like type a" Location.format mtd.pmtd_loc
        | _ -> 
            Str.module_ ?loc:None & Mb.mk ?loc:None (at ?loc:None name)
              (Mod.structure ?loc:None 
               & [%stri [@@@warning "-16"]]
                 :: Str.type_ [gen_ty_module name ps]
                 :: link
                 :: Str.type_ [gen_ty_class ps]
                 :: map (method_ ps) vs
              )
        end
    | _ -> raise_errorf "%a: a signature (sig .. end) is required for [@@@@typeclass]" Location.format mtd.pmtd_loc

  let parse_instance_declaration loc = function
    | PTyp cty ->
        let cty, vars = VarInInstance.replace_var_extensions cty in
        begin match cty with
        | {ptyp_desc = Ptyp_package ({txt; loc}, ps)} ->
            let ps = flip map ps & fun (p,cty) -> match p with
              | {txt=Lident s} -> s, cty
              | {txt;loc} -> raise_errorf "%a Illegal @@@@instance type constraint %a. It must be a simple name." Location.format loc Longident.format txt
            in
            txt, loc, ps, vars
        | _ -> raise_errorf "%a: Illegal @@@@instance payload. It must be [@@instance: (module X with type ...)]" Location.format loc
        end      
    | _ -> raise_errorf "%a: Illegal @@@@instance payload. It must be [@@instance: (module X with type ...)]" Location.format loc
    
      
  (* ((module (struct type a = ... include <m> end) : (<n>.a, <n>.b) <o>._module)) *)
  let dict_module m o ps =
    Exp.constraint_ ?loc:None 
      (Exp.pack ?loc:None &
         Mod.structure & map (fun (p,cty) -> Str.type_ [Type.mk ~manifest: cty (at p)]) ps
                         @ [ Str.include_ { pincl_mod = Mod.ident' ?loc:None (Lident m);
                                            pincl_loc = Location.none; (* CR fix it *)
                                            pincl_attributes = [] }
                         ]  
      )
      (Typ.constr ?loc:None (at ?loc:None & Ldot (Lident o, "_module"))
(*
       & map (fun (p,_) -> Typ.constr ?loc:None (at ?loc:None & Ldot (Lident m, p)) []) ps)
*)
       & map (fun (_,cty) -> cty) ps)
         
  module Dict = struct
      
    (* [param d tvs m] = (d : (<tvs> <m>._module, [%imp has_type <m>.__class__]) Ppx_implicits.t option) *)
    let param d tvs m =
      Pat.constraint_ (Pat.var' d)
      & Typ.(let a = constr (at & Ldot (m, "_module"))
               (map (fun tv -> constr (at & Lident tv) []) tvs)
             in
             let b = Specconv.to_core_type Location.none & Spec.([Has_type (Typ.(constr (at & Ldot (m, "__class__")) []), None)]) in
             [%type: ([%t a], [%t b]) Ppx_implicits.t option])
        
    (* let dict (type a) ?d:(d : (a Numdef.Num._module, [%imp has_type Numdef.Num.__class__]) Ppx_implicits.t option) = *)
  
    (* val (Ppx_implicits.(get (from_Some <d>))) *)
    let functor_arg d = Mod.unpack [%expr Ppx_implicits.(get (from_Some [%e d])) ]
  
    (* ((module (struct type a = ... include <n> end) : (<n>.a, <n>.b) <o>._module)
       
       or
       
       let module M = <n>((val (Ppx_implicits.(get (from_Some d)))))..(..) in
       ((module (struct type a = ... include M end) : (M.a, M.b) <o>._module)
    *)
    let z n ds o ps = match ds with
      | [] -> dict_module n o ps
      | _ -> 
          Exp.letmodule (at "M") (fold_left Mod.apply (Mod.ident (at & Lident n)) & map functor_arg ds)
          & dict_module "M" o ps

    (* let dict (type a) (type b).. ?d1:(d1 : (a m1._module, [%imp has_type m1.__class__]) Ppx_implicits.t option) ?d2:(d2 : (b m2._module, [...]) =
       let module M = <n>((val (Ppx_implicits.(get (from_Some d1))))).. in
       ((module M) : (M.ps1, M.psn) <o>._module)
    *)
    let dict f (* functor *)
             p_mty (* module defines the module type *)
             ps (* parameters of type class *)
             ks (* constraints *)
             vars (* local type names *) =
      let ds = mapi (fun i _ -> "d" ^ string_of_int i) ks in
      let pats = map2 (fun d (tvs, m) -> param d tvs m) ds ks in
      let tvs = concat & map (fun (tvs, _) -> tvs) ks in
      let e = z f (map (fun i -> Exp.ident (at & Lident i)) ds) p_mty ps in
      let e = fold_left2 (fun e d p -> Exp.fun_ (Optional d) None p e) e ds pats in
      [%stri let dict = [%e fold_left (flip Exp.newtype) e (List.map (!@) & tvs @ vars)]]
  end
                                
  (* 
     module ShowInt = struct
       type a = int
       let show  = string_of_int
     end [@@instance Show] 

     =>

     module ShowIntInstance = struct
       let dict : ShowInt.a Show.s = (module ShowInt)
       type __imp_instance_of__ = Show.__class__
     end
  *)
  let gen_instance lid mb ~instance_loc ps vars =
    let cname = match lid with (* Show *)
      | Lident cname -> cname
      | Ldot (_, cname) -> cname
      | _ ->
          raise_errorf "%a: %a is invalid for type class"
            Location.format instance_loc
            Longident.format lid
    in
    let iname = mb.pmb_name.txt in (* ShowInt *)
    let oname = iname ^ "Instance" in (* ShowIntInstance *)
    let _str, ks =
      let rec get_str me = match me.pmod_desc with
        | Pmod_structure str -> str, []
        | Pmod_constraint (me, _) -> get_str me
        | Pmod_functor (_, Some { pmty_attributes = attrs }, me) ->
            begin match
                filter_map (function
                  | ({txt="typeclass"}, PStr [{pstr_desc=Pstr_eval (e,_)}]) -> Some e
                  | ({txt="typeclass"; loc}, _) ->
                      raise_errorf "%a: Invalid syntax of @@typeclass for functor argument.  It must be [@@typeclass <params> <modname>]"
                        Location.format loc
                  | _ -> None) attrs
              with
              | [] -> get_str me
              | [e] ->
                  let k = match e.pexp_desc with
                    | Pexp_apply (tvs, [Nolabel, mp]) ->
                        let tvs =
                          let get_var tv = match tv.pexp_desc with
                            | Pexp_ident {txt=Lident s} -> s
                            | _ ->
                                raise_errorf "%a: Invalid syntax of @@typeclass parameter"
                                  Location.format tv.pexp_loc
                          in
                          match tvs.pexp_desc with
                          | Pexp_tuple es -> map get_var es
                          | _ -> [get_var tvs]
                        in
                        let lid = match mp.pexp_desc with
                          | Pexp_construct ({txt=lid}, None) -> lid
                          | _ ->
                              raise_errorf "%a: Invalid syntax of @@typeclass module"
                                Location.format mp.pexp_loc
                        in
                        tvs, lid
                    | _ -> 
                        raise_errorf "%a: Invalid syntax of @@typeclass for functor argument.  It must be [@@typeclass <params> <modname>]"
                          Location.format e.pexp_loc
                  in
                  let str, ks = get_str me in
                  str, k::ks
              | _ ->
                  raise_errorf "%a: multiple @@typeclass attributes found"
                    Location.format me.pmod_loc
            end
        | Pmod_functor (_, _, me) -> get_str me
        | _ ->
            raise_errorf "%a: Invalid module for @@@@instance"
              Location.format me.pmod_loc
      in
      get_str mb.pmb_expr
    in
    with_gloc mb.pmb_loc & fun () ->
        Str.module_ & Mb.mk (at ~loc:mb.pmb_name.loc oname)
        & Mod.structure ~loc:mb.pmb_expr.pmod_loc
          [ [%stri [@@@warning "-16"] (* We need this for ?imp: *) ]
          ; Dict.dict iname cname ps ks vars
          ; with_gloc instance_loc & fun () ->
            Str.type_ [ Type.mk ~manifest:(Typ.constr (at & Ldot (Lident cname, "__class__")) []) (at "__imp_instance_of__") ]
          ]
end
*)

(* module type S = sig .. end [@@typeclass] *)
(* module M = struct .. end [@@instance C] *)
let extend super =
  let _has_typeclass_attr = function
    | {txt="typeclass"}, PStr [] -> true
    | {txt="typeclass"; loc}, _ ->
        raise_errorf "%a: [@@@@typeclass] must not take payload"
          Location.format loc
    | _ -> false
  in
  let structure self sitems =
    let sitems = flip concat_map sitems & fun sitem ->
      match sitem.pstr_desc with
(*
      | Pstr_modtype mtd when exists has_typeclass_attr mtd.pmtd_attributes ->
          (* module type M = ... [@@typeclass] *)
          (* CR jfuruse: need to remove [@@typeclass] *)
          [ sitem
          (* ; TypeClass.process_module_type_declaration mtd *)
          ; TypeClass.gen_declaration mtd
          ]
      | Pstr_module mb ->
          (* module M = ... [@@instance Show] *)
          begin match 
            flip filter_map mb.pmb_attributes & function 
              | ({txt="instance"; loc}, payload) -> Some (TypeClass.parse_instance_declaration loc payload)
              | _ -> None
          with
          | [] -> [ sitem ]
          | [ (lid, instance_loc, ps, vars) ] ->
              [ sitem
              ; TypeClass.gen_instance lid mb ~instance_loc ps vars
              ]
          | _ -> 
              raise_errorf "%a: multiple [@@@@instance] found"
                Location.format mb.pmb_loc
          end
*)
        | _ -> [sitem]
    in
    super.structure self sitems
  in 
  { super with structure }
end

module Tysize = struct
(* Size of type *)

open Types
open Btype

type t = (int option, int) Hashtbl.t
(** Polynomial. v_1 * 2 + v_2 * 3 + 4 has the following bindings:
    [(Some 1, 2); (Some 3, 2); (None, 4)]
*)

let to_string t =
  let open Printf in
  String.concat " + "
  & Hashtbl.fold (fun k v st ->
    let s = match k,v with
      | None, _ -> sprintf "%d" v
      | Some _, 0 -> ""
      | Some k, 1 -> sprintf "a%d" k
      | Some k, _ -> sprintf "%d*a%d" v k
    in
    s :: st) t []
    
let format ppf t = Format.fprintf ppf "%s" (to_string t)
    
let size ty =
  let open Hashtbl in
  let tbl = create 9 in
  let incr k =
    try
      replace tbl k (find tbl k + 1)
    with
    | Not_found -> add tbl k 1
  in
  let it = 
    { type_iterators with
      it_do_type_expr = (fun it ty ->
        let ty = Ctype.repr ty in
        begin match ty.desc with
        | Tvar _ -> incr (Some ty.id)
        | _ -> incr None
        end;
        begin match ty.desc with
        | Tvariant row -> 
           (* Tvariant may contain a Tvar at row_more even if it is `closed'. *)
           let row = row_repr row in
           if not & static_row row then type_iterators.it_do_type_expr it ty
        | _ -> type_iterators.it_do_type_expr it ty
        end)
    }
  in
  it.it_type_expr it ty;
  unmark_iterators.it_type_expr unmark_iterators ty;
  tbl

let lt t1 t2 =
  (* Comparison of polynomials.
     All the components in t1 must appear in t2 with GE multiplier.
     At least one component in t1 must appear in t2 with GT multiplier.
  *)
  let open Hashtbl in
  try
    fold (fun k1 v1 found_gt ->
      let v2 = find t2 k1 in
      if v1 < v2 then true
      else if v1 = v2 then found_gt
      else raise Exit
      ) t1 false
  with
  | Exit | Not_found -> false

let has_var t =
  try
    Hashtbl.iter (fun k v -> if k <> None && v > 0 then raise Exit) t;
    false
  with
  | Exit -> true
  

end

open Typedtree
open Types

open Format
open List
    
(* CR jfuruse: bad state... *)
(* CR jfuruse: confusing with deriving spec *)                      
let derived_candidates = ref []

(* CR jfuruse: this is very slow, since it computes everything each time. *)
let get_candidates env loc spec ty =
  let f = Spec.candidates env loc spec in
  Candidate.uniq & f ty @ map snd !derived_candidates


module Runtime = struct
  open Longident

   (* Leopard.Implicits.Runtime *)
  let lident_implicits = Ldot (Lident "Leopard", "Implicits")

  let lident_embed = Ldot (lident_implicits, "embed")

  let lident_get = Ldot (lident_implicits, "get")

  let lident_from_Some = Ldot (lident_implicits, "from_Some")

   (** [Leopard.Implicits.Runtime.t] *)
   let is_imp_t_path = function
    | Path.(Pdot(Pdot(Pident{Ident.name="Leopard"},
                      "Implicits", _),
                 "t", _)) -> true
    | _ -> false

   (** [embed e] builds [Ppx_implicits.Runtime.embed <e>] *)
   let embed e =
     let env = e.exp_env in
     let loc = e.exp_loc in
     let f = Typecore.type_exp env (Ast_helper.Exp.ident ~loc {txt=lident_embed; loc}) in
     Forge.Exp.(app f [Nolabel, e])

   (** [get e] builds [Ppx_implicits.get <e>] *)
   let get e =
     let env = e.exp_env in
     let loc = e.exp_loc in
     let f = Typecore.type_exp env (Ast_helper.Exp.ident ~loc {txt=lident_get; loc}) in
     Forge.Exp.(app f [Nolabel, e])
  
   (** [from_Some e] builds [Ppx_implicits.from_Some <e>] *)
   let from_Some e =
     let env = e.exp_env in
     let loc = e.exp_loc in
     let f = Typecore.type_exp env (Ast_helper.Exp.ident ~loc {txt=lident_from_Some; loc}) in
     Forge.Exp.(app f [Nolabel, e])
 end

(** Check it is [(<ty>, <spec>) Ppx_implicits.t] *)
let is_imp_arg_type env ty = 
  match expand_repr_desc env ty with
  | Tconstr (p, [ty; spec], _) when Runtime.is_imp_t_path p ->
      Some (ty, spec)
  | _ -> None

let check_arg env loc l ty
  : ( type_expr
      * Spec.t option
      * (expression -> expression)
      * (expression -> expression)
    ) =
  let default = (ty, None, (fun x -> x), (fun x -> x)) in
  let f ty = match is_imp_arg_type env ty with
    | Some (ty, spec) ->
        let spec = Specconv.from_type_expr env loc spec in
        (ty, Some spec, Runtime.embed, Runtime.get)
    | None -> default
  in
  if not & Btype.is_optional l then f ty
  else begin
    match is_option_type env ty with
    | None ->
        (* optional arg, but the argument type is not optional. strange... *)
        default
    | Some ty -> 
        let ty, spec_opt, conv, unconv = f ty in
        match spec_opt with
        | None -> default
        | Some _ -> 
            (ty, spec_opt,
             (fun e -> Forge.Exp.some env (conv e)),
             (fun e -> unconv (Runtime.from_Some e))
            )
  end 

module Klabel2 = struct
  (* Constraint labels must precede the other arguments *)
  let rec extract env ty = 
    let ty = Ctype.expand_head env ty in
    match repr_desc ty with
    | Tarrow(l, ty1, ty2, _) ->
        begin match check_arg env Location.none l ty1 with
        | (_, Some _, _, _) ->
            let cs, ty = extract env ty2 in
            (l,ty1)::cs, ty
        | _ -> [], ty
        end
    | _ -> [], ty
  
  let rec extract_aggressively env ty =
    let ty = Ctype.expand_head env ty in
    match repr_desc ty with
    | Tarrow(l, ty1, ty2, _) when gen_vars ty1 <> [] ->
        ([], ty)
        :: map
          (fun (cs, ty) -> (l,ty1)::cs, ty)
          (extract_aggressively env ty2)
    | _ -> [[], ty]
  end

(* Fix the candidates by adding type dependent part *)
let extract_candidate spec env loc { Candidate.aggressive; type_ } : ((arg_label * type_expr * Spec.t * (expression -> expression)) list * type_expr) list =
  let f =
    if not aggressive then (fun type_ -> [Klabel2.extract env type_])
    else Klabel2.extract_aggressively env
  in
  flip map (f type_) & fun (args, ty) ->
    (flip map args (fun (l,ty) ->
      let (ty,specopt,conv,_unconv) = check_arg env loc l ty in
      ( l
      , ty
      , (match specopt with
         | Some x -> x
         | None -> spec (* inherit! *))
      , conv))
    , ty)

(*
let extract_candidate spec env loc c = 
  let xs = extract_candidate spec env loc c in
  !!% "Cand: %a@." Candidate.format c;
  !!% "  => @[<v>%a@]@."
    (List.format "@," & fun ppf (subs,ty) ->
      List.format " " (fun ppf (l,ty,_spec,_conv) ->
        Format.fprintf ppf "(%s: %a)"
          l Printtyp.type_scheme ty) ppf subs;
      Format.fprintf ppf " => %a" Printtyp.type_scheme ty) xs;
  xs
*)

module Resolve_result = struct
  type t =
    | Ok of expression list list
    | MayLoop of expression list (** resolution aborted because of possible infinite loops *)

  let concat xs =
    match
      flip partition_map xs & function
        | Ok ys -> `Right ys
        | MayLoop ys -> `Left ys
    with
    | [], oks -> Ok (concat oks)
    | mayloops, _ -> MayLoop (concat mayloops)
end
  
type trace = (Path.t * type_expr) list
(** Used instance history. This is used to check the same instance is
    not used with types with not strictly decreasing size. *)

let rec resolve loc env : (trace * type_expr * Spec.t) list -> Resolve_result.t = function
  | [] -> Resolve_result.Ok [[]] (* one solution with the empty expression set *)
  | (trace,ty,spec)::problems ->
      let cs = get_candidates env loc spec ty in
      if Debug.debug_unif then begin
        !!% "Candidates:@.";
        iter (!!% "  %a@." Candidate.format) cs
      end;
      let cs = concat_map (fun c ->
        map (fun x -> c.Candidate.path, c.Candidate.expr, x) & extract_candidate spec env loc c
      ) cs
      in
      Resolve_result.concat
      & flip map cs
      & resolve_cand loc env trace ty problems

(* CR jfuruse: Once given, loc is constant *)
and resolve_cand loc env trace ty problems (path, expr, (cs,vty)) =

  let org_tysize = Tysize.size ty in 

  match assoc_opt path trace with
  | Some ty' when not & Tysize.(lt org_tysize (size ty')) ->
      (* recursive call of path and the type size is not strictly decreasing *)
      if Debug.debug_unif then begin
        !!% "  Checking %a <> ... using %a ... oops@."
          Printtyp.type_expr ty
          Path.format path;
        !!% "    @[<2>Skip this candidate because of non decreasing %%imp recursive dependency:@ @[<2>%a@ : %a (%s)@ =>  %a (%s)@]@]@." 
          Path.format path
          Printtyp.type_expr ty'
          (Tysize.(to_string & size ty'))
          Printtyp.type_expr ty
          (Tysize.(to_string & size ty));
      end;
      
      Resolve_result.Ok []

  | _ ->
      (* CR jfuruse: Older binding of path is no longer useful. Replace instead of add? *)
      let trace' = (path, ty) :: trace in 

      let ity = Ctype.instance env ty in
     
      let ivty, cs =
        match Ctype.instance_list env (vty::map (fun (_,ty,_spec,_conv) -> ty) cs) with
        | [] -> assert false (* impos *)
        | ivty :: ictys ->
            ivty, map2 (fun (l,_,spec,conv) icty -> (l,icty,spec,conv)) cs ictys
      in

      with_snapshot & fun () ->
        if Debug.debug_unif then begin
          !!% "  Checking %a <> %a, using %a ...@."
            Printtyp.type_expr ity
            Printtyp.type_expr ivty
            Path.format path;
        end;
        match protect & fun () -> Ctype.unify env ity ivty with
        | Error (Ctype.Unify utrace) ->
            if Debug.debug_unif then begin
              !!% "    no@.";
              !!% "      Reason: @[%a@]@."
                (fun ppf utrace -> Printtyp.report_unification_error ppf
                  env utrace
                  (fun ppf -> fprintf ppf "Unification error ")
                  (fun ppf -> fprintf ppf "with"))
                utrace;

              !!% "    Type 1: @[%a@]@." Printtyp.raw_type_expr  ity;
              !!% "    Type 2: @[%a@]@." Printtyp.raw_type_expr  ivty

            end;
            Resolve_result.Ok [] (* no solution *)
                   
        | Error e -> raise e (* unexpected *)

        | Ok _ ->
            if Debug.debug_unif then
              !!% "    ok: %a@." Printtyp.type_expr ity;
            
            let new_tysize = Tysize.size ty in

            if Tysize.(has_var new_tysize
                       && has_var org_tysize
                       && not & lt new_tysize org_tysize)
            then begin
              if Debug.debug_unif then begin
                !!% "    Tysize vars not strictly decreasing %s => %s@."
                  (Tysize.to_string org_tysize)
                  (Tysize.to_string new_tysize)
              end;
                 (* CR jfuruse: this is reported ambiguousity *) 
              Resolve_result.MayLoop [expr]
            end else

              (* Add the sub-problems *)
              let problems = map (fun (_,ty,spec,_conv) -> (trace',ty,spec)) cs @ problems in

              if Debug.debug_unif then
                !!% "    subproblems: [ @[<v>%a@] ]@."
                  (List.format "@," (fun ppf (_,ty,spec,_) ->
                    Format.fprintf ppf "%a / %s"
                      Printtyp.type_scheme ty
                      (Spec.to_string spec))) cs;
              
              match resolve loc env problems with
              | Resolve_result.MayLoop es -> Resolve_result.MayLoop es
              | Resolve_result.Ok res_list ->
                  let build res =
                    let args, res = split_at (length cs) res in
                    Forge.Exp.(app expr (map2 (fun (l,_,_,conv) a -> (l,conv a)) cs args)) :: res
                  in
                  Resolve_result.Ok (map build res_list)

let resolve env loc spec ty = with_snapshot & fun () ->

  if Debug.debug_resolve then !!% "@.RESOLVE: %a@." Location.format loc;

  close_gen_vars ty;

  if Debug.debug_resolve then !!% "  The type is: %a@." Printtyp.type_scheme ty;

  (* CR jfuruse: Only one value at a time so far *)
  match resolve loc env [([],ty,spec)] with
  | Resolve_result.MayLoop es -> 
      raise_errorf "%a:@ The experssion has type @[%a@] which is too ambiguous to resolve this implicit.@ @[<2>The following instances may cause infinite loop of the resolution:@ @[<2>%a@]@]"
        Location.format loc
        Printtyp.type_expr ty
        (* CR jfuruse: should define a function for printing Typedtree.expression *)
        (List.format ",@," format_expression) es
  | Resolve_result.Ok [] ->
      raise_errorf "%a:@ no instance found for@ @[%a@]"
        Location.format loc
        Printtyp.type_expr ty
  | Resolve_result.Ok (_::_::_ as es) ->
      let es = map (function [e] -> e | _ -> assert false (* impos *)) es in
      raise_errorf "%a: This implicit has too ambiguous type:@ @[%a@]@ @[<2>Following possible resolutions:@ @[<v>%a@]"
        Location.format loc
        Printtyp.type_expr ty
        (List.format "@," format_expression) es
  | Resolve_result.Ok [es] ->
      match es with
      | [e] -> Unshadow.Replace.replace e
      | _ -> assert false (* impos *)

let retype env exp expected =
  let uexp = Untypeast.(default_mapper.expr default_mapper) exp in
  Typecore.type_expect env uexp expected
  
(* ?l:None  where (None : (ty,spec) Ppx_implicit.t option) has a special rule *) 
let resolve_omitted_imp_arg loc env a = match a with
  (* (l, None, Optional) means curried *)
  | ((Optional _ as l), Some e) ->
      begin match is_none e with
      | None -> a (* explicitly applied *)
      | Some _ -> (* omitted *)
          let (ty, specopt, conv, _unconv) = check_arg env loc l e.exp_type in
          match specopt with
          | None -> a (* It is not imp arg *)
          | Some spec ->
              let e' = conv (resolve env loc spec ty) in
              let e'' = retype e.exp_env e' e.exp_type in
              (l, Some e'')
      end
  | _ -> a

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  (* Code transformations independent each other must be embeded
     into one  AST mapper, and one part must be scattered into 
     more than two places. Very hard to read. *)

  let create_function_id = 
    let x = ref 0 in
    fun () -> incr x; "__imp__arg__" ^ string_of_int !x

  let is_function_id = String.is_prefix "__imp__arg__"

  let enter_expression e = match e.exp_desc with
    | Texp_apply (f, args) ->
        (* Resolve omitted ?_x arguments *)
        { e with
          exp_desc= Texp_apply (f, map (resolve_omitted_imp_arg f.exp_loc e.exp_env) args) }

    | Texp_function { arg_label=l; param=_; cases= _::_::_; partial= _} when l <> Nolabel ->
        (* Eeek, label with multiple cases? *)
        warnf "%a: Unexpected label with multiple function cases"
          Location.format e.exp_loc;
        e

    | Texp_function { arg_label=l; param; cases= [case]; partial } ->
        let p = case.c_lhs in
        begin match check_arg p.pat_env p.pat_loc l p.pat_type with
        | (_, None, _, _) -> e
        | (type_, Some _spec, _conv, unconv) -> (* CR jfuruse: specs are ignored *)
            let fid = create_function_id () in
            let id = Ident.create fid in
            let path = Path.Pident id in
            let expr = unconv (Forge.Exp.(ident path)) in
            
            derived_candidates := (fid, { Candidate.path; (* <- not actually path. see expr field *)
                                          expr;
                                          type_;
                                          aggressive = false } ) 
                                  :: !derived_candidates;
            let case = { case with
              c_lhs = Forge.(with_loc p.pat_loc & fun () -> Pat.desc (Tpat_alias (p, id, {txt=fid; loc= Ast_helper.ghost p.pat_loc})))} 
            in
            let e = retype e.exp_env { e with exp_desc = Texp_function { arg_label=l; param; cases= [case]; partial } } e.exp_type
            in
            Forge.Exp.mark fid e
        end
    | _ -> e

  let leave_expression e =
    (* Handling derived implicits, part 2 of 2 *)
    match Forge.Exp.partition_marks e & fun txt -> is_function_id txt with
    | [], e -> e
    | [txt], e ->
        (* Hack:
           
           Remove the association of `"__imp_arg__0"` and the candidate of
           `__imp_arg__0` from `derived_candidates`.
        *)
        derived_candidates := filter (fun (fid, _) -> fid <> txt) !derived_candidates;
        e
    | _ -> assert false (* impos *)
end

module Map = TypedtreeMap.MakeMap(MapArg)

let resolve str =
  if !Leopardtype.overload then Map.map_structure str
  else str
