(**************************************************************************)
(*                                                                        *)
(*                                 OCaml                                  *)
(*                                                                        *)
(*             Xavier Leroy, projet Cristal, INRIA Rocquencourt           *)
(*                                                                        *)
(*   Copyright 2002 Institut National de Recherche en Informatique et     *)
(*     en Automatique.                                                    *)
(*                                                                        *)
(*   All rights reserved.  This file is distributed under the terms of    *)
(*   the GNU Lesser General Public License version 2.1, with the          *)
(*   special exception on linking described in the file LICENSE.          *)
(*                                                                        *)
(**************************************************************************)

(* The batch compiler *)

open Misc
open Config
open Format
open Typedtree
open Compenv

(* Compile a .mli file *)

(* Keep in sync with the copy in compile.ml *)

let tool_name = "ocamlopt"

let pp_out = function
  | `Structure str when !Clflags.as_pp_text ->
      Format.printf "%a@." Pprintast.structure str
  | `Structure str ->
      output_string stdout Config.ast_impl_magic_number;
      output_value stdout !Location.input_name;
      output_value stdout str;
      close_out stdout
  | `Signature sg when !Clflags.as_pp_text ->
      Format.printf "%a@." Pprintast.signature sg
  | `Signature sg ->
      output_string stdout Config.ast_intf_magic_number;
      output_value stdout !Location.input_name;
      output_value stdout sg

let interface ppf sourcefile outputprefix =
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env () in
  let ast = Pparse.parse_interface ~tool_name ppf sourcefile in
  if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
  if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;

  let initial_env = Compmisc.leopard_init initial_env in
  (* -no-trans *)
  if !Clflags.no_trans then pp_out (`Signature ast) else

  Timings.(time_call (Typing sourcefile)) (fun () ->
    let tsg = Typemod.type_interface sourcefile initial_env ast in
    if !Clflags.dump_typedtree then fprintf ppf "%a@." Printtyped.interface tsg;
    let sg = tsg.sig_type in
    if !Clflags.print_types then
      Printtyp.wrap_printing_env initial_env (fun () ->
          fprintf std_formatter "%a@."
            Printtyp.signature (Typemod.simplify_signature sg));
    ignore (Includemod.signatures initial_env sg sg);
    Typecore.force_delayed_checks ();
    Warnings.check_fatal ();

    (* retype *)
    let tsg =
      if !Clflags.no_retype then tsg
      else begin
        let ast = Untypeast.untype_signature tsg in
        Compmisc.init_path false;
        Env.set_unit_name modulename;
        let initial_env = Compmisc.initial_env () in
        let tsg = Typemod.type_interface sourcefile initial_env ast in
        let sg = tsg.sig_type in
        ignore (Includemod.signatures initial_env sg sg);
        Typecore.force_delayed_checks ();
        Warnings.check_fatal ();
        tsg
      end
    in

    (* -as-pp *)
    if !Clflags.as_pp then pp_out (`Signature (Untypeast.untype_signature tsg)) else

    if not !Clflags.print_types then begin
      let deprecated = Builtin_attributes.deprecated_of_sig ast in
      let sg =
        Env.save_signature ~deprecated sg modulename (outputprefix ^ ".cmi")
      in
      Typemod.save_signature modulename tsg outputprefix sourcefile
        initial_env sg ;
    end
  )

(* Compile a .ml file *)

let print_if ppf flag printer arg =
  if !flag then fprintf ppf "%a@." printer arg;
  arg

let (++) x f = f x
let (+++) (x, y) f = (x, f y)

let implementation ~backend ppf sourcefile outputprefix =
  let source_provenance = Timings.File sourcefile in
  Compmisc.init_path true;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  Compilenv.reset ~source_provenance ?packname:!Clflags.for_package modulename;
  let cmxfile = outputprefix ^ ".cmx" in
  let objfile = outputprefix ^ ext_obj in
  let comp ast =
    let env = Compmisc.leopard_init env in
    (* -no-trans *)
    if !Clflags.no_trans then begin
      let ast =
        ast
        ++ print_if ppf Clflags.dump_parsetree Printast.implementation
        ++ print_if ppf Clflags.dump_source Pprintast.structure
      in
      Warnings.check_fatal ();
      pp_out (`Structure ast) 
    end else

    let (typedtree, coercion) =
      ast
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Timings.(time (Typing sourcefile))
          (Typemod.type_implementation sourcefile outputprefix modulename env)
      ++ print_if ppf Clflags.dump_typedtree
          Printtyped.implementation_with_coercion
    in

    (* retype *)      
    let (typedtree, coercion) =
      if !Clflags.no_retype then (typedtree, coercion)
      else begin
        Warnings.check_fatal ();
        Stypes.dump (Some (outputprefix ^ ".annot"));

        Leopardfeatures.without_leopard (fun () ->
            Compmisc.init_path false;
            Env.set_unit_name modulename;
            let env = Compmisc.initial_env() in
            Untypeast.untype_structure typedtree
            ++ Timings.(time (Typing sourcefile))
              (Typemod.type_implementation sourcefile outputprefix modulename env))
      end
    in

    (* -as-pp *)
    if !Clflags.as_pp then begin
      Warnings.check_fatal ();
      Stypes.dump (Some (outputprefix ^ ".annot"));
      pp_out (`Structure (Untypeast.untype_structure typedtree))
    end else

    if not !Clflags.print_types then begin
      if Config.flambda then begin
        if !Clflags.classic_inlining then begin
          Clflags.default_simplify_rounds := 1;
          Clflags.use_inlining_arguments_set Clflags.classic_arguments;
          Clflags.unbox_free_vars_of_closures := false;
          Clflags.unbox_specialised_args := false
        end;
        (typedtree, coercion)
        ++ Timings.(time (Timings.Transl sourcefile)
            (Translmod.transl_implementation_flambda modulename))
        ++ Timings.time (Timings.Generate sourcefile)
          (fun { Lambda.module_ident; main_module_block_size;
                 required_globals; code } ->
          ((module_ident, main_module_block_size), code)
          +++ print_if ppf Clflags.dump_rawlambda Printlambda.lambda
          +++ Simplif.simplify_lambda sourcefile
          +++ print_if ppf Clflags.dump_lambda Printlambda.lambda
          ++ (fun ((module_ident, size), lam) ->
              Middle_end.middle_end ppf ~source_provenance
                ~prefixname:outputprefix
                ~size
                ~filename:sourcefile
                ~module_ident
                ~backend
                ~module_initializer:lam)
          ++ Asmgen.compile_implementation_flambda ~source_provenance
            outputprefix ~required_globals ~backend ppf;
          Compilenv.save_unit_info cmxfile)
      end
      else begin
        Clflags.use_inlining_arguments_set Clflags.classic_arguments;
        (typedtree, coercion)
        ++ Timings.(time (Transl sourcefile))
            (Translmod.transl_store_implementation modulename)
        ++ print_if ppf Clflags.dump_rawlambda Printlambda.program
        ++ Timings.(time (Generate sourcefile))
            (fun program ->
              { program with
                Lambda.code = Simplif.simplify_lambda sourcefile
                  program.Lambda.code }
              ++ print_if ppf Clflags.dump_lambda Printlambda.program
              ++ Asmgen.compile_implementation_clambda ~source_provenance
                outputprefix ppf;
              Compilenv.save_unit_info cmxfile)
      end
    end;
    Warnings.check_fatal ();
    Stypes.dump (Some (outputprefix ^ ".annot"))
  in
  try comp (Pparse.parse_implementation ~tool_name ppf sourcefile)
  with x ->
    Stypes.dump (Some (outputprefix ^ ".annot"));
    remove_file objfile;
    remove_file cmxfile;
    raise x
