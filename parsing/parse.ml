(***********************************************************************)
(*                                                                     *)
(*                                OCaml                                *)
(*                                                                     *)
(*            Xavier Leroy, projet Cristal, INRIA Rocquencourt         *)
(*                                                                     *)
(*  Copyright 1996 Institut National de Recherche en Informatique et   *)
(*  en Automatique.  All rights reserved.  This file is distributed    *)
(*  under the terms of the Q Public License version 1.0.               *)
(*                                                                     *)
(***********************************************************************)

(* Entry points in the parser *)

let () = Lexer.set_preprocessor Indent.init Indent.preprocess

(* Skip tokens to the end of the phrase *)

let rec skip_phrase lexbuf =
  try
    match Lexer.token lexbuf with
      Parser.SEMISEMI | Parser.EOF -> ()
    | _ -> skip_phrase lexbuf
  with
    | Lexer.Error (Lexer.Unterminated_comment _, _)
    | Lexer.Error (Lexer.Unterminated_string, _)
    | Lexer.Error (Lexer.Unterminated_string_in_comment _, _)
    | Lexer.Error (Lexer.Illegal_character _, _) -> skip_phrase lexbuf
;;

let maybe_skip_phrase lexbuf =
  if Parsing.is_current_lookahead Parser.SEMISEMI
  || Parsing.is_current_lookahead Parser.EOF
  then ()
  else skip_phrase lexbuf

let wrap parsing_fun lexbuf =
  try
    Lexer.init ();
    let ast = parsing_fun Lexer.token lexbuf in
    Parsing.clear_parser();
    ast
  with
  | Lexer.Error(Lexer.Illegal_character _, _) as err
    when !Location.input_name = "//toplevel//"->
      skip_phrase lexbuf;
      raise err
  | Syntaxerr.Error _ as err
    when !Location.input_name = "//toplevel//" ->
      maybe_skip_phrase lexbuf;
      raise err
  | Parsing.Parse_error | Syntaxerr.Escape_error ->
      let loc = Location.curr lexbuf in
      if !Location.input_name = "//toplevel//"
      then maybe_skip_phrase lexbuf;
      raise(Syntaxerr.Error(Syntaxerr.Other loc))

let implementation = wrap Parser.implementation
and interface = wrap Parser.interface
and toplevel_phrase = wrap Parser.toplevel_phrase
and use_file = wrap Parser.use_file
and core_type = wrap Parser.parse_core_type
and expression = wrap Parser.parse_expression
and pattern = wrap Parser.parse_pattern


(* Desugaring *)

open Ast_mapper
open Parsetree

let mapper = List.fold_right (fun f st -> f st) 
  [ ]
  default_mapper

let implementation lexbuf = 
  mapper.structure mapper @@ implementation lexbuf

let interface lexbuf = 
  mapper.signature mapper @@ interface lexbuf

let top = function
  | Ptop_def s -> 
      Ptop_def (mapper.structure mapper s)
  | (Ptop_dir _ as p) -> p

let toplevel_phrase lexbuf = top @@ toplevel_phrase lexbuf

let use_file lexbuf = List.map top @@ use_file lexbuf

let core_type lexbuf = 
  mapper.typ mapper @@ core_type lexbuf

let expression lexbuf = 
  mapper.expr mapper @@ expression lexbuf

let pattern lexbuf = 
  mapper.pat mapper @@ pattern lexbuf
