(* Extension of compilier_libs modules *)

open Leopardutils
open List

module XLongident : sig
  val format : Longident.t Format.fmt
  val to_string : Longident.t -> string
  module Set : Set.S with type elt = Longident.t
end = struct
  open Longident
  open Format
  
  (* [Pprintast.longident] is not exposed in OCaml 4.05.0 *)
  let rec format ppf = function
    | Lident n -> pp_print_string ppf n
    | Ldot (p, name) -> fprintf ppf "%a.%s" format p name
    | Lapply (p1, p2) -> fprintf ppf "%a(%a)" format p1 format p2
                           
  let to_string l = asprintf "%a" format l

  module Set = Set.Make(struct type t = Longident.t let compare = compare end)
end

module Longident = struct
  include Longident
  include XLongident
end

module XLocation : sig
  val ghost : Location.t -> Location.t
  val merge : Location.t -> Location.t -> Location.t
  val format : Location.t Format.fmt
  module Open : sig
    val raise_errorf
      : ?loc:Location.t
      -> ?sub:Location.error list
      -> ?if_highlight:string
      -> ('a, Format.formatter, unit, 'b) format4 -> 'a

    type 'a loc = 'a Location.loc = { txt : 'a; loc : Location.t }

    val at : ?loc:Location.t -> 'a -> 'a loc
    val (!@) : 'a -> 'a loc
  end
end = struct
  open Location
  let ghost l = { l with loc_ghost = true }
  let merge t1 t2 = { t1 with loc_end = t2.loc_end }

  (* Location's printers do something wierd internally and cancels
     things already printed:

       [Format.eprintf "Error %a %a@." Path.format p Location.print loc]

     In the above code, "Error <path>" is somehow gone. 
  *)
  let format ppf loc =
    let open Format in
    let open Lexing in
    let (msg_file, msg_line, msg_chars, msg_to, _msg_colon) =
      ("File \"", "\", line ", ", characters ", "-", ":")
    in
    let (file, line, startchar) = get_pos_info loc.loc_start in
    let endchar = loc.loc_end.pos_cnum - loc.loc_start.pos_cnum + startchar in
    if file = "//toplevel//" then begin
      fprintf ppf "Characters %i-%i"
        loc.loc_start.pos_cnum loc.loc_end.pos_cnum
    end else begin
      fprintf ppf "%s@{<loc>%a%s%i" msg_file print_filename file msg_line line;
      if startchar >= 0 then
        fprintf ppf "%s%i%s%i" msg_chars startchar msg_to endchar;
      fprintf ppf "@}"
    end
       
  module Open = struct
    let raise_errorf = Location.raise_errorf
    type 'a loc = 'a Location.loc = { txt : 'a; loc : Location.t }

    let at ?loc txt = 
      let loc = match loc with 
        | None -> !Ast_helper.default_loc
        | Some loc -> loc
      in
      { txt; loc }
    
    let (!@) x = at x
  end
end

module Location = struct
  include Location
  include XLocation
end

open Location.Open

module XParsetree : sig
  open Parsetree
  val iter_core_type : (core_type -> unit) -> core_type -> unit
  val constrs_in_type_declaration : type_declaration -> Longident.t list
  val group_type_declarations
    : type_declaration list
    -> type_declaration list list * type_declaration list
  val is_gadt : type_declaration -> bool
  val tvars_of_core_type : core_type -> string list

  val sig_module_of_stri : structure_item -> signature_item
    
  val is_marked : string -> expression -> bool * expression
  val mark : string -> expression -> expression
end = struct
  (* We cannot include Parsetree since it lacks implementation *)
  open Parsetree

  let iter_core_type f ty =
    let open Ast_iterator in
    let super = default_iterator in
    let typ self ty =
      super.typ self ty;
      f ty
    in
    let i = { super with typ } in
    i.typ i ty
      
  (* referred constrs and classes *)   
  let constrs_in_core_type_ ty =
    let s = ref Longident.Set.empty in
    let add l = s := Longident.Set.add l !s in
    iter_core_type
      (fun ty -> match ty.ptyp_desc with
         | Ptyp_constr ({txt}, _) -> add txt
         | Ptyp_class ({txt}, _) -> add txt
         | _ -> ())
      ty;
    !s

  let constrs_in_core_type ty =
    Longident.Set.elements & constrs_in_core_type_ ty

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
      | [] -> Either.Right td.ptype_name.txt
      | ns -> Either.Left (td.ptype_name.txt, ns)) tds
    in
    let groups = sccs graph in
    (List.map (List.map (flip List.assoc alist)) groups,
     List.map (flip List.assoc alist) nonrecs)
      
  let is_gadt type_decl = match type_decl.ptype_kind with
    | Ptype_variant constrs -> List.exists (fun c -> c.pcd_res <> None) constrs
    | _ -> false

  let tvars_of_core_type cty =
    (* Using mapper for iterator is redundant, but easiest way *)
    (* XXX We should use Ast_iterator *)
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

  (* XXX not used ?*)
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

  let is_marked m sexp = 
    let b = List.exists (fun ({txt}, c) -> txt = m && c = PStr []) sexp.pexp_attributes in
    b, { sexp with pexp_attributes = List.filter (fun ({txt},_) -> txt <> m) sexp.pexp_attributes }
    
  let mark m sexp = 
    { sexp with pexp_attributes = ({ txt=m
                                   ; loc= sexp.pexp_loc}, PStr []) 
                                  :: sexp.pexp_attributes }
end

include Location.Open

