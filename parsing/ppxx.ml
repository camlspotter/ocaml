open Parsetree
open Asttypes
open Ast_helper

(* (@@) is too strong *)
external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"

let (!!%) = Format.eprintf

let mkloc ?loc txt = 
  let loc = match loc with 
      | None -> !Ast_helper.default_loc
      | Some loc -> loc
  in
  { txt; loc }

let lid ?loc s = mkloc ?loc & Longident.parse s

let partition p = List.partition (fun ({txt}, _payload) -> p txt)

module Location = struct
  include Location
  open Lexing

  let compare_position p1 p2 =
    if p1.pos_fname <> p2.pos_fname then None
    else Some (compare p1 p2)

  let merge t1 t2 =
    let start = 
      match compare_position t1.loc_start t2.loc_start with
      | None -> None
      | Some (-1) -> Some t1
      | Some 0 -> Some t1
      | Some 1 -> Some t2
      | _ -> assert false
    in
    let end_ = 
      match compare_position t1.loc_end t2.loc_end with
      | None -> None
      | Some (-1) -> Some t2
      | Some 0 -> Some t2
      | Some 1 -> Some t1
      | _ -> assert false
    in
    match start, end_ with
    | Some start, Some end_ -> 
        { loc_start = start.loc_start; 
          loc_end = end_.loc_end; 
          loc_ghost = t1.loc_ghost || t2.loc_ghost }
    | _ -> none

  let ghost l = { l with loc_ghost = true }
end

module Typ = struct
  include Typ
  let new_var =
    let cntr = ref 0 in
    fun ?loc ?attrs prefix -> 
      incr cntr;
      var ?loc ?attrs & prefix ^ string_of_int !cntr
  let ref_ ?loc ?attrs ty = 
    constr ?loc ?attrs (mkloc ?loc & Longident.Lident "ref") [ty]
  let int ?loc ?attrs () = constr ?loc ?attrs (lid ?loc "int") []
end

module Exp = struct
  include Exp
  let string ?loc ?attrs s = constant ?loc ?attrs & Const_string (s, None)
  let int ?loc ?attrs i = constant ?loc ?attrs & Const_int i
  let bool ?loc ?attrs b = construct ?loc ?attrs (lid ?loc (if b then "true" else "false")) None
  let var ?loc ?attrs s = ident ?loc ?attrs & mkloc ?loc & Longident.Lident s
  let id ?loc ?attrs s = ident ?loc ?attrs & mkloc ?loc & Longident.parse s
  let option ?loc ?attrs = function
    | None -> construct ?loc ?attrs (lid ?loc "None") None
    | Some e -> construct ?loc ?attrs (lid ?loc "Some") (Some e)
  let object_ ?loc ?attrs flds = object_ ?loc ?attrs (Cstr.mk (Pat.any ()) flds)
  let seqs = function
    | [] -> assert false
    | x::xs -> List.fold_right (fun x st -> sequence x st) xs x

  let pervasives ?loc ?attrs s = id ?loc ?attrs & "Pervasives." ^ s

  let parse s =
    try
      Parser.parse_expression Lexer.token (Lexing.from_string s)
    with
    | _e -> failwith (Printf.sprintf "parse fail: %s" s)

  let ignore_ e = apply (id "Pervasives.ignore") ["", e]
  let magic e = apply (id "Obj.magic") ["", e]
  let assert_false ?loc ?attrs () = assert_ ?loc ?attrs & bool ?loc false
  let raise_ name argopt = 
    apply (pervasives "raise") [ "", construct (lid name) argopt  ]

end

module Pat = struct
  include Pat
  let var ?loc ?attrs s = var ?loc ?attrs (mkloc ?loc s)
end

module ExpPat = struct
  let var ?loc ?attrs s = (Exp.var ?loc ?attrs s, Pat.var ?loc ?attrs s)
end

module Cf = struct
  include Cf

  let method_concrete ?loc ?attrs name ?(priv=false) ?(override=false) e = 
    Cf.method_ ?loc ?attrs name (if priv then Private else Public)
      (Cfk_concrete ((if override then Override else Fresh), e))
  let method_virtual ?loc ?attrs name ?(priv=false) cty = 
    Cf.method_ ?loc ?attrs name (if priv then Private else Public)
      (Cfk_virtual cty)
end

module Cstr = struct
  let mk ?(self= Pat.any ()) fields = Cstr.mk self fields
end

let ppx_name = ref "ppx name is not set"

let ppx_errorf ?(loc = Location.none) ?(sub = []) ?(if_highlight = "") =
  Printf.ksprintf (fun msg ->
    let e = { Location.loc; msg; sub; if_highlight } in
    Format.eprintf "Error at %s: %a@." !ppx_name Location.report_error e;
    exit 1)

(*
open Ast_mapper

let test mapper fname = 
  try
    if Filename.check_suffix fname ".ml" then (* .mlt ? *)
      let str = Pparse.parse_implementation ~tool_name:"ppx" Format.err_formatter fname in
      let str = mapper.structure mapper str in
      Pprintast.structure Format.std_formatter str
    else if Filename.check_suffix fname ".mli" then 
      let sg = Pparse.parse_interface ~tool_name:"ppx" Format.err_formatter fname in
      let sg = mapper.signature mapper sg in
      Pprintast.signature Format.std_formatter sg
    else assert false;
    Format.fprintf Format.std_formatter "@."
  with
  | Syntaxerr.Error e ->
      !!% "%a@." Syntaxerr.report_error e

let run name mapper = 
  ppx_name := name;
  let debug = ref false in
  let rev_files = ref [] in 
  Arg.parse 
    [ "-debug", Arg.Set debug, "debug mode which can take .ml/.mli then print the result"
    ]
    (fun s -> rev_files := s :: !rev_files) (* will be handled by [Ast_mapper.apply] *)
    name;
  match !debug, List.rev !rev_files with
  | true, files ->
      List.iter (test mapper) files
  | false, [infile; outfile] ->
      Ast_mapper.apply ~source:infile ~target:outfile mapper
  | _ -> 
      failwith @@ name ^ " infile outfile"
*)
