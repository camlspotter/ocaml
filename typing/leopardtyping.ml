(* Extension of compilier_libs modules *)

open Leopardutils
open Asttypes
open Leopardparsing
open List

module XIdent : sig
  val format : Ident.t Format.fmt
  val format_verbose : Ident.t Format.fmt
end = struct
  open Ident
  open Format
  let format ppf id = pp_print_string ppf id.name
  let format_verbose ppf id = fprintf ppf "%s/%d" id.name id.stamp
end

module Ident = struct
  include Ident
  include XIdent
end

module XPath : sig
  val format : Path.t Format.fmt
  val format_verbose : Path.t Format.fmt
  val to_string : Path.t -> string
end = struct
  open Path
  open Format

  let format = Printtyp.path

  let rec format_verbose ppf = function
    | Pident id -> Ident.format_verbose ppf id
    | Pdot (p, name, n) -> fprintf ppf "%a.%s__%d" format_verbose p name n
    | Papply (p1, p2) -> fprintf ppf "%a(%a)" format_verbose p1 format_verbose p2

  let to_string l = asprintf "%a" format l
end

module Path = struct
  include Path
  include XPath
end

module XCtype : sig
  (**
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
  open Types

  val expand_head : Env.t -> type_expr -> type_expr

  val apply
    : Env.t
    -> type_expr list
    -> type_expr
    -> type_expr list
    -> type_expr

  val moregeneral
    : Env.t
    -> bool
    -> type_expr
    -> type_expr
    -> bool

  val with_def : (unit -> 'a) -> 'a
end = struct
  include Ctype
    
  let expand_head env ty = expand_head env & correct_levels ty
  let apply env tys ty tys2 =
    apply env (map correct_levels tys) (correct_levels ty) (map correct_levels tys2)
  let moregeneral env b ty1 ty2 = moregeneral env b (correct_levels ty1) (correct_levels ty2)

  let with_def f =
    begin_def ();
    let res = f () in
    end_def ();
    res
end

module Ctype = struct
  include Ctype
  include XCtype
end
  
module XTypes : sig
  open Types

  val repr_desc : type_expr -> type_desc

  val expand_repr_desc : Env.t -> type_expr -> type_desc

  val with_snapshot : (unit -> 'a) -> 'a

  val is_constr : Env.t -> type_expr -> (Path.t * type_expr list) option

  val is_option_type : Env.t -> type_expr -> type_expr option

  val gen_vars : type_expr -> type_expr list

  (** Create a type which can be unified only with itself *)
  val create_uniq_type : unit -> type_expr
    
  val close_gen_vars : type_expr -> unit

end = struct
  open Types
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

module Types = struct
  include Types
  include XTypes
end

module XEnv : sig
  val scrape_sg : Env.t -> Types.module_declaration -> Types.signature

  type fold = 
    | Class of Ident.t * Path.t * Types.class_declaration
    | Cltype of Ident.t * Path.t * Types.class_type_declaration
    | Modtype of Ident.t * Path.t * Types.modtype_declaration
    | Module of Ident.t * Path.t * Types.module_declaration
    | Type of Ident.t * Path.t * Types.type_declaration
    | Typext of Ident.t * Path.t * Types.extension_constructor
    | Value of Ident.t * Path.t * Types.value_description

  val fold_module
    : Env.t
    -> Path.t
    -> 'a
    -> ('a
        -> fold
        -> 'a)
    -> 'a

  class dummy_module
    : Env.t
      -> Path.t
      -> Types.module_type ->
      object
        method lookup_module : string -> Path.t
        method lookup_type   : string -> Path.t * Types.type_declaration
        method lookup_value  : string -> Path.t
      end

  val values_of_module : recursive:bool -> Env.t -> Location.t -> Path.t -> Path.t list

  val get_opens : Env.t -> Path.t list

  val dump_summary : Env.t -> unit
  (** Print type, module and open information to stderr *)
    
end = struct
  open Types
  open Path
  open Env
      
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

  type fold = 
    | Class of Ident.t * Path.t * Types.class_declaration
    | Cltype of Ident.t * Path.t * Types.class_type_declaration
    | Modtype of Ident.t * Path.t * Types.modtype_declaration
    | Module of Ident.t * Path.t * Types.module_declaration
    | Type of Ident.t * Path.t * Types.type_declaration
    | Typext of Ident.t * Path.t * Types.extension_constructor
    | Value of Ident.t * Path.t * Types.value_description

  let fold_module env path init f =
    (* Format.eprintf "fold_module %a@." Printtyp.path path; *)
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
              Value (id, pathfix p, vdesc)
          | Sig_type (id, td, _) ->
              let p = Env.lookup_type (lid id) env' in
              Type (id, pathfix p, td)
          | Sig_typext (id, ec, _) ->
              let p = Env.lookup_type (lid id) env' in
              Typext (id, pathfix p, ec)
          | Sig_module (id, mdecl, _) ->
              let p = Env.lookup_module ~load:false (lid id) env' in
              Module (id, pathfix p, mdecl)
          | Sig_modtype (id, _) ->
              let p, mdtd = Env.lookup_modtype (lid id) env' in
              Modtype (id, pathfix p, mdtd)
          | Sig_class (id, _, _) ->
              let p, cd = Env.lookup_class (lid id) env' in
              Class (id, pathfix p, cd)
          | Sig_class_type (id, _, _) ->
              let p, ctd = Env.lookup_cltype (lid id) env' in
              Cltype (id, pathfix p, ctd)
        in
        f st x) init sg

  (* XXX I believe now fold_module is for it *)
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

  (* XXX pretty sure we do not need it any more *)
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
  
  (* XXX pretty sure we do not need it any more *)
  let values_of_module ~recursive env loc path =
    let mdecl = check_module env loc path in
    values_of_module ~recursive env path mdecl

  (** Opened modules in the current environment *)
  let get_opens env =
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
    get & summary env

  let dump_summary env =
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
end

module Env = struct
  include Env
  include XEnv
end

module Mangle : sig
  val mangle : string -> string
  val unmangle : string -> (string, [> `Failed_unmangle of string ]) result
end = struct
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
end

module XParser : sig
  val expression_from_string : string -> (Parsetree.expression, [> `Parse of string]) result
end = struct
  let expression_from_string s = 
    let lexbuf = Lexing.from_string s in
    try Ok (Parser.parse_expression Lexer.token lexbuf) with
    | _ -> Error (`Parse s)
end

module Parser = struct
  include Parser
  include XParser
end

open List
open Longident
open Types

module XTypedtree : sig
  open Typedtree

  val format_expression : expression Format.fmt
  val is_none : expression -> Types.type_expr option

  val check_constructor_is_for_path : Env.t -> string -> Path.t -> unit

  val some : Env.t -> expression -> expression
  val app : Env.t ->
               expression ->
               (Asttypes.arg_label * expression) list ->
               expression

  val add_attribute : string -> Parsetree.structure_item -> expression -> expression
  (** Add [@<name> <sitem>] to an expression *)

  val filter_attributes 
    : (attribute -> ('a, attribute) Either.t)  (*+ The predicate to filter the attributes *)
    -> expression
    -> 'a list * expression
    (** Filter out the attributes of the name which matches with the predicate.
        It returns the matched attributes and the expression without them.
    *)

end = struct
  open Typedtree

  let format_expression ppf e =
    Pprintast.expression ppf
    & (* Typpx. *) Untypeast.(default_mapper.expr default_mapper) e
  
  let is_none e =
    match e.exp_desc with
    | Texp_construct ({Location.txt=Lident "None"}, _, []) -> 
        begin match is_option_type e.exp_env e.exp_type with
        | None ->
            !!% "is_none: the input is type-corrupted@."; assert false
        | Some ty -> Some ty
        end
    | _ -> None
  

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
      
  let some ev e =
    check_constructor_is_for_path ev "Some" Predef.path_option;
    Typecore.option_some e

  let app env funct args =
    match args with
    | [] -> funct
    | _ ->
        let fty = funct.exp_type in
        let atys = map (function
            | (l,e) -> l, e.exp_type) args
        in
        let rety = Btype.newgenvar () in
        let apty = fold_right (fun (l,ty) t -> Btype.newgenty & Tarrow (l, ty, t, Cok)) atys rety in
        !!% "@[<2>App:@ @[%a@]@ @[%a@]@]@." Printtyp.raw_type_expr fty Printtyp.raw_type_expr apty;
        Ctype.unify env fty apty;
        !!% "@[<2>App: =>@ @[%a@]@ @[%a@]@." Printtyp.raw_type_expr fty Printtyp.raw_type_expr apty;
        { exp_desc = Texp_apply (funct, map (fun (l,a) -> (l,Some a)) args)
        ; exp_loc = Location.none
        ; exp_extra = []
        ; exp_type = rety
        ; exp_env = env
        ; exp_attributes = []
        }
    
  open Asttypes
  
  let add_attribute key v e =
    { e
      with exp_attributes= ( {txt=key; loc= Location.ghost e.exp_loc}
                           , Parsetree.PStr [v]) 
                           :: e.exp_attributes }
        
  let filter_attributes f e =
    let xs, exp_attributes = partition_map f e.exp_attributes in
    xs,
    { e with exp_attributes }
end

module Typedtree = struct
  include Typedtree
  include XTypedtree

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
      
  let some ev e =
    check_constructor_is_for_path ev "Some" Predef.path_option;
    Typecore.option_some e

  let app env funct args =
    match args with
    | [] -> funct
    | _ ->
        let fty = funct.exp_type in
        let atys = map (function
            | (l,e) -> l, e.exp_type) args
        in
        let rety = Btype.newgenvar () in
        let apty = fold_right (fun (l,ty) t -> Btype.newgenty & Tarrow (l, ty, t, Cok)) atys rety in
        !!% "App:  %a  %a@." Printtyp.type_scheme fty Printtyp.type_scheme apty;
        Ctype.unify env fty apty;
        !!% "App:  => %a  %a@." Printtyp.type_scheme fty Printtyp.type_scheme apty;
        { exp_desc = Texp_apply (funct, map (fun (l,a) -> (l,Some a)) args)
        ; exp_loc = Location.none
        ; exp_extra = []
        ; exp_type = rety
        ; exp_env = env
        ; exp_attributes = []
        }
    
end

module Forge = struct
  (* We should not use this module as possible, since it only fakes
     Typedtree data with incorrect types. *)
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
    (* XXX This is incorrect *)
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

module Tysize : sig 
  (** Size of type *)
  type t
  val lt : t -> t -> bool
  val to_string : t -> string
  val format : Format.formatter -> t -> unit
  val has_var : t -> bool
  val size : Types.type_expr -> t
end = struct
  
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
