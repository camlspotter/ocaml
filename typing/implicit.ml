open Leopardutils
open Leopardparsing
open Leopardtyping

open List
open Asttypes

let debug_resolve = Sys.env_exist "LEOPARD_IMPLICITS_DEBUG_RESOLVE"

(*
module Klabel : sig

  val is_klabel : Asttypes.arg_label -> [> `Normal | `Optional ] option

  val extract :
    Env.t
    -> Types.type_expr
    -> (Asttypes.arg_label * Types.type_expr) list * Types.type_expr

  val extract_aggressively :
    Env.t
    ->  Types.type_expr
    -> ((Asttypes.arg_label * Types.type_expr) list * Types.type_expr) list

end = struct
  (** Constraint labels *)

  open Types

  (* Constraint labels must start with '_' *)
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

  (* In the agressive mode, any argument with gen_vars can be constraint arguments *)
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
*)
    
module Candidate : sig

  type t = {
    path : Path.t;
    expr : Typedtree.expression;
    type_ : Types.type_expr;
    aggressive : bool;
  }

  val format : t Format.fmt

  val uniq : t list -> t list

  val cands_of_module :
    Env.t
    -> Location.t
    -> bool (*+ recursive *)
    -> Path.t (*+ module path *)
    -> t list
    
  val mods_direct :
    Env.t
    -> Longident.t
    -> Path.t list
      
  val mods_opened :
    Env.t
    -> Longident.t
    -> Path.t list
      
  val mods_related :
    Env.t
    -> Types.type_expr
    -> Path.t list
      
end = struct
  (*

    Instance search space specification DSL, mangling to and back from
    OCaml type definitions.

  *)
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
      Typedtree.format_expression c.expr
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

  (* Finds out modules named `lids` under the specified module or the current scope *)
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
        (* !!% "module_lids_in_open_path %a@." Path.format open_; *)
        match
          try
            (* None is returned only when the `path` referrs a functor.
               If the path is not found, it raises `Not_found` *)
            Env.open_signature Fresh open_ env (* I doubt it does not work *)
          with
          | Not_found -> None
        with
        | None -> assert false
        | Some env ->
            flip filter_map lids (fun lid ->
                try
                  (* This may find `lid` defined out of `open_` *)
                  let p = Env.lookup_module ~load:true (*?*) lid env in
                  if Path.is_prefix_of open_ p then Some p else None
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

  let cands_of_module env loc recursive mpath =
    let paths = Env.values_of_module ~recursive env loc mpath in
    map (default_candidate_of_path env) paths

  let mods_direct env lid =
    try
      [Env.lookup_module ~load:true lid env]
    with
    | Not_found ->
        (* Derived implicit does not require the candidate space *)
        []

  let mods_opened env lid =
    let opens = Env.get_opens env in
    if debug_resolve then begin
      !!% "debug_resolve: mods_opened opened paths@.";
      flip iter opens & !!% "  %a@." Path.format
    end;
    let paths =
      concat_map (module_lids_in_open_path env [lid])
      & None :: map (fun x -> Some x) opens
    in
    if debug_resolve then begin
      !!% "debug_resolve: mods_opened cand modules@.";
      flip iter paths & !!% "  %a@." Path.format
    end;
    paths

(*
  let cand_name rex f =
    f () |> filter (fun x -> Re_pcre.pmatch ~rex & Longident.to_string & (* Typpx. *) Untypeast.lident_of_path x.path)
*)

  let mods_related env hint_ty =
    (* XXX no inf loop even with rectypes? *)
    Format.eprintf "hint type: %a@." Printtyp.type_scheme hint_ty;
    let rec find_paths ty = match Types.repr_desc ty with
      | Types.Tconstr (p, _, _) ->
          let mp = match p with
            | Path.Pdot (p, _, _) -> p
            | Path.Pident _ -> Path.Pident (Ident.create_persistent "Pervasives") (* XXX move to a library module *)
            | _ -> assert false
          in
          mp :: begin match Env.find_type_expansion p env with
          | (_, body, _) -> find_paths body
          | exception Not_found -> []
          end
      | _ -> []
    in
    let paths = find_paths hint_ty in
    if debug_resolve then begin
      !!% "cand_related: modules: @[%a@]@."
        (Format.list "@," Path.format) paths;
    end;
    paths
end

module Spec : sig
  (**

    Instance search space specification DSL, magling to and back from
    OCaml type definitions.

  *)

  type module_specifier = 
    | Direct of Longident.t
      (** [M] is the module accessible by name [M] in the resolution context. *)
    | Opened of Longident.t
      (** [opened M] are the modules accessible by name [P.M] in the resolution context,
          where [P] are opened.
      *)
    | Related of Parsetree.core_type * Types.type_expr option
      (** [(related : 'a)], it is module [M] if the type ['a] is instantiated to
          a data type defined in module [M], for example, [M.t].  If the data type
          is an alias of data type defined in another module [N], [N] is also traversed.
          This alias expansion is performed recursively. *)

  type filter =
    | Substr of string (** Only the values whose name contain the string in it *) 

  type traversal = 
    { module_specifier : module_specifier
    ; recursive : bool (** Includes the values defined in the sub-modules,
                           or just the toplevel values defined in the module *)
    ; filters : filter list
    ; aggressive : bool (** How sub constraints are obtained *)
    }

  type t = traversal list

  val to_string : t -> string

  val candidates : Env.t -> Location.t -> t -> Candidate.t list
end = struct

  type module_specifier = 
    | Direct of Longident.t
      (** [M] is the module accessible by name [M] in the resolution context. *)
    | Opened of Longident.t
      (** [opened M] are the modules accessible by name [P.M] in the resolution context,
          where [P] are opened.
      *)
    | Related of Parsetree.core_type * Types.type_expr option
      (** [(related : 'a)], it is module [M] if the type ['a] is instantiated to
          a data type defined in module [M], for example, [M.t].  If the data type
          is an alias of data type defined in another module [N], [N] is also traversed.
          This alias expansion is performed recursively. *)

  type filter =
    | Substr of string (** Only the values whose name contain the string in it *) 

  type traversal = 
    { module_specifier : module_specifier
    ; recursive : bool (** Includes the values defined in the sub-modules,
                           or just the toplevel values defined in the module *)
    ; filters : filter list
    ; aggressive : bool
    }

  type t = traversal list

  let to_string =
    let rec t trs = String.concat ", " (map aggressive trs)
    and aggressive ({ aggressive } as t) =
      if aggressive then Printf.sprintf "aggressive (%s)" (filters t)
      else filters t
    and filters ({ filters } as t) =
      let f filter str = match filter with
        | Substr s -> Printf.sprintf "substr %S (%s)" s str
      in
      List.fold_right f filters & traversal t
    and traversal { module_specifier=ms; recursive } =
      if recursive then module_specifier ms
      else Printf.sprintf "just (%s)" (module_specifier ms)
    and module_specifier = function
      | Direct l -> Longident.to_string l
      | Opened l -> Printf.sprintf "opened (%s)" & Longident.to_string l
      | Related (cty,_) -> Format.asprintf "(related : %a)" Pprintast.core_type cty
    in
    t

  (** "Dynamic" means that candidates are dependent on the target type *)
  let _is_static = function
    | Opened _ -> true
    | Direct _ -> true
    | Related (_,_) -> false

  (** spec to candidates *)

  open Candidate

  let mods env = function
    | Direct x -> mods_direct env x
    | Opened x -> mods_opened env x
    | Related (_,Some ty) -> mods_related env ty
    | Related (_,None) -> assert false

  let apply_filter f cs =
    let eval_filter f c = match f with
      | Substr s -> String.is_substring ~needle:s & Path.to_string c.path
    in
    filter (eval_filter f) cs

  let cands env loc { module_specifier; recursive; filters; aggressive } =
    let cs =
      List.fold_right apply_filter filters
      & concat_map (Candidate.cands_of_module env loc recursive) & mods env module_specifier
    in
    if aggressive then map (fun c -> { c with aggressive = true }) cs
    else cs

  let candidates env loc t = uniq & concat_map (cands env loc) t
end

module Specconv : sig
  (** Spec conversion

      Specs cannot appear in programs as they are.
      This module provides conversions between them and types and expressions.
  *)

  open Types

  val from_type_expr : Env.t -> Location.t -> type_expr -> Spec.t
end = struct

  open Parsetree
  open Types

  open Spec

  let prefix = "Spec_"
  let prefix_len = String.length prefix

  (** Extract type parts so that they can be encoded in the parameter part
      of a variant constructor.
  *)
  let get_type_components =
    let rec t = function
      | xs -> concat_map traversal xs
    and traversal { module_specifier } = match module_specifier with
      | Direct (_l) -> []
      | Opened (_l) -> []
      | Related (cty,_) -> [cty]
    in
    t

  (** Assign types of variant constructor parameter to spec *)
  let assign_type_components tys t0 =
    let rec t tys = function
      | xs ->
          let tys, rev_xs =
            fold_left (fun (tys,rev_xs) x ->
              let tys, x = traversal tys x in
              tys, x :: rev_xs) (tys,[]) xs
          in
          tys, (rev rev_xs)

    and traversal tys ({ module_specifier } as tr) = match module_specifier with
      | Direct _ -> tys, tr
      | Opened _ -> tys, tr
      | Related (_cty,Some _) -> assert false (* impos *)
      | Related (cty,None) ->
          begin match tys with
          | ty::tys -> tys, { tr with module_specifier = Related (cty,Some ty) }
          | _ -> assert false (* impos *)
          end
    in
    match t tys t0 with
    | [], t0 -> t0
    | _ -> assert false (* impos *)

  let mangle x =
    (prefix ^ Mangle.mangle (to_string x),
     get_type_components x)

  (* CR jfuruse: need tests *)
  let unmangle_spec_string loc s =
    if not & String.is_prefix prefix s then raise_errorf ~loc "Mangled spec string does not start with \"Spec_\": %s" s;
    let s = String.sub s prefix_len (String.length s - prefix_len) in
    Mangle.unmangle s

  let from_string = XParser.expression_from_string

  let from_expression _env e =
    let open Longident in
    try
      let rec t e = match e.pexp_desc with
        | Pexp_tuple xs -> map filters xs
        | _ -> [filters e]
      and filters e = match e.pexp_desc with
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "aggressive"} },
                      [(Nolabel, e1)] ) ->
            let t = filters e1 in
            { t with aggressive = true }
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "substr"} },
                      [(Nolabel, e1); (Nolabel, e2)] ) ->
            let f = match e1.pexp_desc with
              | Pexp_constant (Pconst_string (s, _)) -> Substr s
              | _ -> 
                  raise_errorf ~loc:e1.pexp_loc "Filter must be a string constant: "
                    Pprintast.expression e1
            in
            let t = filters e2 in
            { t with filters = f :: t.filters }
        | _ -> traversal e
      and traversal e = match e.pexp_desc with
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "aggressive"} },
                      [(Nolabel, e1)] ) ->
            let t = traversal e1 in
            { t with aggressive = true }
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "just"} },
                      [Nolabel, e] ) -> 
            { module_specifier = module_specifier e
            ; recursive = true
            ; filters = []
            ; aggressive = false
            }
        | _ ->
            { module_specifier = module_specifier e
            ; recursive = false
            ; filters = []
            ; aggressive = false
            }
      and module_specifier e = match e.pexp_desc with
        | Pexp_construct ({txt=lid}, None) -> Direct lid
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "opened"} },
                      [Nolabel, {pexp_desc= Pexp_construct ({txt=lid}, None)}]) -> Opened lid
        | Pexp_constraint ({pexp_desc= Pexp_ident {txt=Lident "related"}}, cty) ->
            Related (cty, None)
(*
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "name"} },
                      [ Nolabel, { pexp_desc = Pexp_constant (Pconst_string (s, _)) }
                      ; Nolabel, e ] ) -> Name (s, Re_pcre.regexp s, t2 e)
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "has_type"} }, args ) ->
             begin match args with
            | [Nolabel, e] ->
                begin match e.pexp_desc with
                | Pexp_ident lid ->
                    Has_type (Ast_helper.Typ.constr lid [], None)
                | _ -> raise_errorf ~loc:e.pexp_loc "has_type must take a type path"
                end
            | _ -> raise_errorf ~loc:e.pexp_loc "has_type must take just one argument"
            end

        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "deriving"} }, args ) ->
            begin match args with
            | [Nolabel, e] ->
                begin match get_lid e with
                | Some lid -> Deriving lid
                | None -> raise_errorf ~loc:e.pexp_loc "deriving must take an module path"
                end
            | _ -> raise_errorf ~loc:e.pexp_loc "deriving must take just one argument"
            end
        | Pexp_apply( { pexp_desc= Pexp_ident {txt=Lident "ppxderive"} }, args ) ->
            begin match args with
            | [Nolabel, e] ->
                begin match e.pexp_desc with
                | Pexp_constraint (e, cty) -> PPXDerive (e, cty, None)
                | _ -> raise_errorf ~loc:e.pexp_loc "ppxderive must take (e : t)"
                end
            | _ -> raise_errorf ~loc:e.pexp_loc "ppxderive must take just one argument"
            end
*)
        | _ ->
            raise_errorf ~loc:e.pexp_loc "Illegal spec module_specifier: %a"
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
    | `String s -> raise_errorf ~loc "%s" s
    | `Failed_unmangle s ->
        raise_errorf ~loc "Illegal spec encoding: %S" s
    | `Parse s ->
        raise_errorf ~loc "Spec parse failed: %S" s
    | `ParseExp (_, s) ->
        raise_errorf ~loc "Spec parse failed: %s" s

  let from_payload_ env = function
    | PStr s -> from_structure env s
    | _ -> Error (`String "spec must be an OCaml expression")

  let from_payload loc x = match from_payload_ Env.empty (* dummy *) x with
    | Error err -> error loc err
    | Ok spec -> spec

  (* typed world *)

  (********************* NEW ENCODING USING POLYVARIANT ***************)

  (** Obtain a spec from a type expr.

      ex. [ `Spec__28related_20_3a_20'a_29 of < l0 : 'a > ] => (related : 'a)
  *)
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
            & unmangle_spec_string loc l
              >>= from_string
              >>= from_expression env
        | _ ->
            raise_errorf ~loc "Illegal type for implicit spec"
        end
    | _ ->
        raise_errorf ~loc "Illegal type for implicit spec (not a polymorphic variant type)"

  let to_core_type _loc spec = (* XXX we should use loc *)
    let open Ast_helper in
    let mangled, ctys = mangle spec in
    let label n = !@ ("l" ^ string_of_int n) in
(* XXX vars should not be quantified for related!
    let make_meth_type cty =  (* quantify cty *)
      Typ.poly (map (!@) & XParsetree.tvars_of_core_type cty) cty
    in
*)
    let make_meth_type cty = cty in
    let oty = Typ.object_ (mapi (fun n cty -> Otag (label n, [], make_meth_type cty)) ctys) Closed
    in
    Typ.variant [ Parsetree.Rtag ({txt=mangled; loc=Location.none (* XXX *)}, [], false, [oty]) ] Closed None

  let from_payload_to_core_type : Location.t -> payload -> core_type = fun loc x ->
    to_core_type loc & from_payload loc x

  let () = Leopardppx.Imp.from_payload_to_core_type_forward := from_payload_to_core_type

end

open Typedtree
open Types
open Format

(* CR jfuruse: bad state... *)
(* CR jfuruse: confusing with deriving spec *)
let derived_candidates = ref []

(* CR jfuruse: this is very slow, since it computes everything each time. *)
let get_candidates env loc spec =
  Candidate.uniq & Spec.candidates env loc spec @ map (fun (_,x,_,_) -> x) !derived_candidates

module Runtime = struct
  open Longident

  (* Here, we cannot perform Ctype.unify easily,
     which may change the levels of generalized type variables.

     After type inference finishes, we often have ill-formed types ty['a]
     in type annotations, where 'a is generalized, but ty is not.

     Unification beween such ty['a] with a completely generalized type ty'
     makes 'a not generalized. Because of the ungeneralized type level of ty.
  *)

  (* Leopard.Implicits.Runtime *)
  let lident_implicits = Ldot (Lident "Leopard", "Implicits")

  (* Leopard.Implicits.Runtime.embed *)
  let lident_embed     = Ldot (lident_implicits, "embed")

  (* Leopard.Implicits.Runtime.get *)
  let lident_get       = Ldot (lident_implicits, "get")

  (* Leopard.Implicits.Runtime.from_Some *)
  let lident_from_Some = Ldot (lident_implicits, "from_Some")

  (** [Leopard.Implicits.t] *)
  let is_imp_t_path = function
    | Path.Pdot(Path.Pdot(Path.Pident {Ident.name="Leopard"},
                          "Implicits", _),
                "t", _) -> true
    | _ -> false


  let get_ident env loc lid =
    Typecore.type_exp env (Ast_helper.Exp.ident ~loc {txt=lid; loc})

  let illtyped_app e args =
    { e with
      exp_desc= Texp_apply (e, List.map (fun (l,x) -> (l,Some x)) args)
    ; exp_loc = Location.none
    ; exp_extra = []
    ; exp_attributes = []
    }

  (** [embed e] builds [Leopard.Implicits.embed <e>] *)
  let embed e =
    let f = get_ident e.exp_env e.exp_loc lident_embed in
    (* Typedtree.app e.exp_env f [Nolabel, e] *)
    illtyped_app f [Nolabel, e]

  (** [get e] builds [Leopard.Implicits.get <e>] *)
  let get e =
    let f = get_ident e.exp_env e.exp_loc lident_get in
    (* Typedtree.app e.exp_env f [Nolabel, e] *)
    illtyped_app f [Nolabel, e]

  (** [from_Some e] builds [Leopard.Implicits.from_Some <e>] *)
  let from_Some e =
    let f = get_ident e.exp_env e.exp_loc lident_from_Some in
    illtyped_app f [Nolabel, e]
end

(** Check it is `(<ty>, <spec>) Ppx_implicits.t` and returns `Some (ty, spec)` *)
let is_imp_arg_type env ty =
  match expand_repr_desc env ty with
  | Tconstr (p, [ty; spec], _) when Runtime.is_imp_t_path p ->
      Some (ty, spec)
  | _ -> None

let is_imp_arg env loc l ty
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
             (fun e -> Typedtree.some env (conv e)),
             (fun e -> unconv (Runtime.from_Some e))
            )
  end

module Klabel2 = struct
  (* Constraint labels must precede the other arguments *)
  let rec extract env ty =
    let ty = Ctype.expand_head env ty in
    match repr_desc ty with
    | Tarrow(l, ty1, ty2, _) ->
        begin match is_imp_arg env Location.none l ty1 with
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
      let (ty,specopt,conv,_unconv) = is_imp_arg env loc l ty in
      ( l
      , ty
      , (match specopt with
         | Some x -> x
         | None -> spec (* This is obtained by aggressive. Let's inherit! *))
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
        | Ok ys -> Either.Right ys
        | MayLoop ys -> Either.Left ys
    with
    | [], oks -> Ok (concat oks)
    | mayloops, _ -> MayLoop (concat mayloops)
end

type trace = (Path.t * type_expr) list
(** Used instance history. This is used to check the same instance is
    not used with types with not strictly decreasing size. *)

let resolve loc env (problems : (trace * type_expr * Spec.t) list) : Resolve_result.t =
  let rec resolve = function
    | [] -> Resolve_result.Ok [[]] (* one solution with the empty expression set *)
    | p::ps -> resolve_a_problem p ps

  and resolve_a_problem (trace,ty,spec) problems =
    let cs = get_candidates env loc spec in
    if debug_resolve then begin
      !!% "Candidates:@.";
      iter (!!% "  %a@." Candidate.format) cs
    end;
    let cs = concat_map (fun c ->
        map (fun x -> c.Candidate.path, c.Candidate.expr, x)
        & extract_candidate spec env loc c
      ) cs
    in
    Resolve_result.concat
    & map (try_cand trace ty problems) cs

  and try_cand trace ity problems (path, expr, (cs,vty)) =

    let org_tysize = Tysize.size ity in

    match assoc_opt path trace with
    | Some ty' when not & Tysize.(lt org_tysize (size ty')) ->
        (* recursive call of path and the type size is not strictly decreasing *)
        if debug_resolve then begin
          !!% "  Checking %a <> ... using %a ... oops@."
            Printtyp.type_expr ity
            Path.format path;
          !!% "    @[<2>Skip this candidate because of non decreasing %%imp recursive dependency:@ @[<2>%a@ : %a (%s)@ =>  %a (%s)@]@]@."
            Path.format path
            Printtyp.type_expr ty'
            (Tysize.(to_string & size ty'))
            Printtyp.type_expr ity
            (Tysize.(to_string & size ity));
        end;

        Resolve_result.Ok []

    | _ ->
        (* CR jfuruse: Older binding of `path` in `trace` is no longer useful.
           Replace instead of add?
        *)
        let trace' = (path, ity) :: trace in

        (* Instantiate the candidate types *)
        let ivty, cs =
          match Ctype.instance_list env (vty::map (fun (_,ty,_spec,_conv) -> ty) cs) with
          | [] -> assert false (* impos *)
          | ivty :: ictys ->
              ivty, map2 (fun (l,_,spec,conv) icty -> (l,icty,spec,conv)) cs ictys
        in

        with_snapshot & fun () ->
          if debug_resolve then begin
            !!% "  Checking %a <> %a, using %a ...@."
              Printtyp.type_expr ity
              Printtyp.type_expr ivty
              Path.format path;
          end;
          match protect & fun () -> Ctype.unify env ity ivty with
          | Error (Ctype.Unify utrace) ->
              if debug_resolve then begin
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

          | Ok () ->
              if debug_resolve then
                !!% "    ok: %a@." Printtyp.type_expr ity;

              let new_tysize = Tysize.size ity in

              if Tysize.(has_var new_tysize
                         && has_var org_tysize
                         && not & lt new_tysize org_tysize)
              then begin
                if debug_resolve then begin
                  !!% "    Tysize vars not strictly decreasing %s => %s@."
                    (Tysize.to_string org_tysize)
                    (Tysize.to_string new_tysize)
                end;
                (* CR jfuruse: this is reported ambiguousity *)
                Resolve_result.MayLoop [expr]
              end else

                (* Add the sub-problems *)
                let problems = map (fun (_,ty,spec,_conv) -> (trace',ty,spec)) cs @ problems in

                if debug_resolve then
                  !!% "    subproblems: [ @[<v>%a@] ]@."
                    (List.format "@," (fun ppf (_,ty,spec,_) ->
                      Format.fprintf ppf "%a / %s"
                        Printtyp.type_scheme ty
                        (Spec.to_string spec))) cs;

                match resolve problems with
                | Resolve_result.MayLoop es -> Resolve_result.MayLoop es
                | Resolve_result.Ok res_list ->
                    (* split the sub problems *)
                    let build res =
                      let args, res = split_at (length cs) res in
                      Typedtree.app expr.exp_env expr (map2 (fun (l,_,_,conv) a -> (l,conv a)) cs args) :: res
                    in
                    Resolve_result.Ok (map build res_list)
  in
  resolve problems

(* Shadowing check

   Shadow check can find the derived_candidates, since they are added to `env`.
*)
let shadow_check env loc e =
  let shadowed = ref [] in
  let module I = TypedtreeIter.MakeIterator(struct
      include TypedtreeIter.DefaultIteratorArgument
      let enter_expression e = match e.exp_desc with
        | Texp_ident (p, _, _) ->
            begin match Env.value_accessibility env p with
            | `NotFound -> raise_errorf ~loc "Shadow check: NotFound for %a" Path.format p
            | `ShadowedBy _ as s -> shadowed := (p,s) :: !shadowed
            | `Accessible _ -> ()
            end
        | _ -> ()
    end)
  in
  I.iter_expression e;
  match !shadowed with
  | [] -> ()
  | _ ->
      let format_shadow ppf = function
        | (_, (`Accessible _ | `NotFound)) -> assert false
        | p, `ShadowedBy (mv, p', loc) ->
            Format.fprintf ppf "%a is shadowed by %s %a defined at %a"
              Path.format p
              (match mv with `Value -> "value" | `Module -> "module")
              Path.format p'
              Location.format loc
      in
      raise_errorf ~loc "@[<2>The implicit argument is resolved to@ @[%a@], but the following values are shadowed:@ @[<v>%a@]@]"
        Typedtree.format_expression e
        (Format.list "@," format_shadow) !shadowed

let resolve env loc spec ty = with_snapshot & fun () ->

  if debug_resolve then !!% "@.RESOLVE: %a@." Location.format loc;

  (* Protect the generalized type variables by temporarily unified by unique types,
     so that they can be unifiable only with themselves.
  *)
  close_gen_vars ty;

  if debug_resolve then !!% "  The type is: %a@." Printtyp.type_scheme ty;

  (* CR jfuruse: Only one value at a time so far *)
  let open Resolve_result in
  match resolve loc env [([],ty,spec)] with
  | MayLoop es ->
      raise_errorf ~loc "The experssion has type @[%a@] which is too ambiguous to resolve this implicit.@ @[<2>The following instances may cause infinite loop of the resolution:@ @[<2>%a@]@]"
        Printtyp.type_expr ty
        (* CR jfuruse: should define a function for printing Typedtree.expression *)
        (List.format ",@," format_expression) es
  | Ok [] ->
      raise_errorf ~loc "No instance found for@ @[%a@]"
        Printtyp.type_expr ty
  | Ok (_::_::_ as es) ->
      let es = map (function [e] -> e | _ -> assert false (* impos *)) es in
      raise_errorf ~loc "This implicit has too ambiguous type:@ @[%a@]@ @[<2>Following possible resolutions:@ @[<v>%a@]"
        Printtyp.type_expr ty
        (List.format "@," format_expression) es
  | Ok [es] ->
      match es with
      | [] | _::_::_ -> assert false (* impos *)
      | [e] ->
          shadow_check env loc e;
          e

let resolve env loc spec ty =
  let e = resolve env loc spec ty in
  (* Now we replay the type unifications! *)
  (* But generalized variables must be protected! *)
  let gvars = gen_vars ty in
  let gvar_descs = List.map (fun gv -> gv, gv.desc) gvars in
  close_gen_vars ty;
  if debug_resolve then
    !!% "Replaying %a against %a@."
      Typedtree.format_expression e
      Printtyp.type_scheme ty;
  let ue = Untypeast.(default_mapper.expr default_mapper) e in
  let te = Typecore.type_expression env ue in
  if debug_resolve then
    !!% "Replaying %a : %a against %a@."
      Typedtree.format_expression te
      Printtyp.type_scheme te.exp_type
      Printtyp.type_scheme ty;
  Ctype.unify env te.exp_type ty;
  (* recover gvars *)
  List.iter (fun (gv, desc) -> gv.desc <- desc; gv.level <- Btype.generic_level) gvar_descs;
  if debug_resolve then
    !!% "Replay result: %a@."
      Printtyp.type_scheme te.exp_type;
  te

(* ?l:None  where (None : (ty,spec) Ppx_implicit.t option) has a special rule *)
let resolve_omitted_imp_arg loc env a = match a with
  (* (l, None, Optional) means curried *)
  | ((Optional _ as l), Some e) ->
      (* Add derived candidates to the environment *)
      let env =
        List.fold_left (fun env (_,_,id,vdesc) -> Env.add_value id vdesc env) env !derived_candidates in
      begin match is_none e with
      | None -> a (* explicitly applied *)
      | Some _t -> (* omitted *)
          let (ty, specopt, conv, _unconv) = is_imp_arg env loc l e.exp_type in
          match specopt with
          | None -> a (* It is not imp arg *)
          | Some spec ->
              let e' = conv (resolve env loc spec ty) in
              (* for retyping, e.exp_env is not suitable, since
                 it is made by Typecore.option_none with Env.initial_safe_string
              *)
              let e'' = { e' with exp_env = env; exp_type = e.exp_type } in
              (l, Some e'')
      end
  | _ -> a

module MapArg : TypedtreeMap.MapArgument = struct
  include TypedtreeMap.DefaultMapArgument

  (* Code transformations independent each other must be embeded
     into one  AST mapper, and one part must be scattered into
     more than two places. Very hard to read. *)

  let create_imp_arg_name =
    let x = ref 0 in
    fun () -> incr x; "__imp__arg__" ^ string_of_int !x

  let is_function_id = String.is_prefix "__imp__arg__"

  let un_some e = match e.exp_desc with
    | Texp_construct ({txt=Longident.Lident "Some"}, _, [e]) -> Some e
    | _ -> None

  (* Fix 0 arg application *)
  let fix_no_args e = match e.exp_desc with
    | Texp_apply (x, []) -> { e with exp_desc = x.exp_desc }
    | _ -> e

  (* XXX weak. Only checked by the Longidents *)
  (* Leopard.Implicits.get (Leopard.Implicits.embed e'') => e'' *)
  let get_embed e = match e.exp_desc with
    | Texp_apply ( { exp_desc= Texp_ident (_, {txt=lid}, _) }, [ Nolabel, Some e' ] ) when lid = Runtime.lident_get ->
        begin match e'.exp_desc with
        | Texp_apply ( { exp_desc= Texp_ident (_, {txt=lid}, _) }, [ Nolabel, Some e'' ] ) when lid = Runtime.lident_embed -> e''
        | _ -> e
        end
    | _ -> e

  let opt e = match e.exp_desc with
    | Texp_apply ( f, args ) ->
        begin match f.exp_desc with
        | Texp_ident (_path, _lidloc, { val_kind= Val_prim { Primitive.prim_name = "%imp" }}) ->
            (* <%imp> l:x ... *)
            let l = match repr_desc f.exp_type with
              | Tarrow (l, _, _, _) -> l
              | _ -> assert false
            in
            begin match
              partition_map (fun (l',a) ->
                  if l = l' then Either.Left a
                  else Either.Right (l', a)
                ) args
            with
            | [Some a], args ->
                fix_no_args & begin match un_some a with
                  | Some a ->
                      (* <%imp> ?l:Some a x ..   =>  Runtime.get a x .. *)
                      { e with
                        exp_desc = Texp_apply (get_embed & Runtime.get a, args)
                      ; exp_type = e.exp_type }
                  | None ->
                      (* <%imp> ?l:a x ..
                                 => Runtime.get (Runtime.from_Some a) x ..
                      *)
                      { e with
                        exp_desc = Texp_apply (Runtime.get & Runtime.from_Some a, args)
                      ; exp_type = e.exp_type }
                  end
            | _ -> e
            end
        | _ -> e
        end
    | _ -> e

  (* XXX This does not work at all. In
      [let double ?d x = add x x],
     the omitted argument of [add] and [d] do not share the same type variables
     at this stage!
   *)
  let add_derived_candidate e case p type_ unconv =
    (* This is an imp arg *)

    (* Build  fun ?d:(d as __imp_arg__) -> *)

    let loc = Location.ghost p.pat_loc in
    let name = create_imp_arg_name () in
    (* p ->     ===>   (p as fname) -> *)
    let lident = Longident.Lident name in
    let id = Ident.create name in
    let path = Path.Pident id in
    let vdesc =
      { val_type = p.pat_type
      ; val_kind = Val_reg
      ; val_loc = loc
      ; val_attributes = []  }
    in
    let expr =
      unconv
      & { exp_desc = Texp_ident (path, {txt=lident; loc=Location.none}, vdesc)
        ; exp_loc = Location.none
        ; exp_extra = []
        ; exp_type = p.pat_type
        ; exp_env = p.pat_env
        ; exp_attributes = []
        }
    in
    derived_candidates := (name
                          , { Candidate.path; (* <- not actually path. see expr field *)
                              expr;
                              type_;
                              aggressive = false }
                          , id
                          , vdesc
                          )
                          :: !derived_candidates;
    let p' = { p with pat_desc = Tpat_alias (p, id, {txt=name; loc})} in
    let case = { case with c_lhs = p' } in
    let e = match e.exp_desc with
      | Texp_function f ->
          { e with exp_desc = Texp_function { f with cases= [case] }}
      | _ -> assert false
    in

    (* __imp_arg__ will be used for the internal resolutions *)
    (* XXX needs a helper module *)
    Typedtree.add_attribute
      "leopard_mark"
      (Ast_helper.(Str.eval (Exp.constant (Parsetree.Pconst_string (name, None))))) e

  let clean_derived_candidates e =
    let open Parsetree in
    (* Handling derived implicits, part 2 of 2 *)
    let is_mark ({txt}, payload as a) =
      if txt <> "leopard_mark" then Either.Right a
      else
        match payload with
        (* XXX need a helper *)
        | PStr [{pstr_desc=Pstr_eval({pexp_desc=Pexp_constant (Pconst_string (s, _))}, _)}] when is_function_id s -> Either.Left s
        | _ -> Either.Right a
    in
    match Typedtree.filter_attributes is_mark e with
    | [], e -> e
    | [txt], e ->
        (* Hack:

           Remove the association of `"__imp_arg__0"` and the candidate of
           `__imp_arg__0` from `derived_candidates`.
        *)
        derived_candidates := filter (fun (fid, _, _, _) -> fid <> txt) !derived_candidates;
        e
    | _ -> assert false (* impos *)

  let enter_expression e = match e.exp_desc with
      | Texp_apply (f, args) ->
          (* Resolve omitted ?_x arguments *)
          (* XXX cleanup *)
          opt { e with
                exp_desc= Texp_apply (f, map (resolve_omitted_imp_arg f.exp_loc e.exp_env) args) }

      | Texp_function { arg_label=l; param=_; cases= _::_::_; partial= _} when l <> Nolabel ->
          (* Eeek, label with multiple cases? *)
          warnf "%a: Unexpected label with multiple function cases"
            Location.format e.exp_loc;
          e

      | Texp_function { arg_label=l; cases= [case] } ->
          let p = case.c_lhs in
          begin match is_imp_arg p.pat_env p.pat_loc l p.pat_type with
          | (_, None, _, _) -> e
          | (type_, Some _spec, _conv, unconv) ->
              (* CR jfuruse: specs are ignored *)
              (* This is an imp arg *)
              (* Build  fun ?d:(d as __imp_arg__) -> *)
              add_derived_candidate e case p type_ unconv
          end
      | _ -> e

  let leave_expression e = clean_derived_candidates e
end

module Map = TypedtreeMap.MakeMap(MapArg)

let resolve str =
  if !Leopardfeatures.overload then Map.map_structure str
  else str
