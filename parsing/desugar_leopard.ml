(* [%leopard {| modified syntax |} %] => Vanilla OCaml *)

(* desugaring of Haskellish value-type declaration *)
open Asttypes
open Parsetree
(* open Syntaxerr *)
open Ast_mapper
open Ppxx

let format_position ppf p =
  let open Lexing in
  Format.fprintf ppf "%s %d %d %d" 
  p.pos_fname p.pos_lnum p.pos_bol p.pos_cnum

let format_location ppf l = 
  let open Location in
  Format.fprintf ppf "%a - %a"
    format_position l.loc_start
    format_position l.loc_end

module Lexing = struct
  include Lexing
  let from_string_with_pos s pos =
    let lexbuf = Lexing.from_string s in
    lexbuf.lex_abs_pos <- pos.pos_cnum;
    lexbuf.lex_start_p <- pos;
    lexbuf.lex_curr_p <- pos;
    lexbuf
end

let special_ext = function
  | {txt="leopard"},  PStr [ stri ] ->
    begin match stri.pstr_desc with
      | Pstr_eval (e, _) ->
          begin match e.pexp_desc with
          | Pexp_constant (Const_string (s, kopt)) -> 
              let open Lexing in
              let open Location in
              let offset = match kopt with
                | None -> 1 (* "aaa" *)
                | Some s -> 
                    (* {name|...|name} *)
                    String.length s + 2
              in
              let start = e.pexp_loc.loc_start in
              let pos = { start with pos_cnum = start.pos_cnum + offset } in
              let lexbuf = Lexing.from_string_with_pos s pos in
              Some (s, lexbuf)
          | _ -> None
          end
      | _ -> None
      end
  | _ -> None

let do_special_ext org ext ps =
  match special_ext ext with
  | None -> org
  | Some (_s, lexbuf) ->
      Lexer.init ();
      ps lexbuf

let desugar_expr exp = match exp.pexp_desc with
  | Pexp_extension ext -> do_special_ext exp ext Parse.expression
  | _ -> exp

let desugar_core_type cty =
  match cty.ptyp_desc with
  | Ptyp_extension ext -> do_special_ext cty ext Parse.core_type
  | _ -> cty
      
let desugar_pat pat =
  match pat.ppat_desc with
  | Ppat_extension ext -> do_special_ext pat ext Parse.pattern
  | _ -> pat
      
let desugar_structure str =
  let desugar_structure_item stri =
    match stri.pstr_desc with
    | Pstr_extension (ext, _) ->
        do_special_ext [stri] ext Parse.implementation
    | _ -> [stri]
  in
  List.concat & List.map desugar_structure_item str

let desugar_signature sg =
  let desugar_signature_item sigi =
    match sigi.psig_desc with
    | Psig_extension (ext, _) ->
        do_special_ext [sigi] ext Parse.interface
    | _ -> [sigi]
  in
  List.concat & List.map desugar_signature_item sg

let extend super =
  let expr      self x = super.expr      self & desugar_expr      x in
  let typ       self x = super.typ       self & desugar_core_type x in
  let pat       self x = super.pat       self & desugar_pat       x in
  let structure self x = super.structure self & desugar_structure x in
  let signature self x = super.signature self & desugar_signature x in
  { super with expr; typ; pat; structure; signature }

let () = Ast_mapper.extend_builtin_mapper extend
