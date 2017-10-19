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
open Format
open Typedtree
open Compenv

(* Compile a .mli file *)

(* Keep in sync with the copy in optcompile.ml *)

let tool_name = "ocamlc"

let interface ppf sourcefile outputprefix =
  let pp_out sg =
    if !Clflags.pp_text then begin
      Format.printf "%a@." Pprintast.signature sg
    end else begin
      let write_ast oc ast =
        output_string oc Config.ast_intf_magic_number;
        output_value oc !Location.input_name;
        output_value oc ast;
      in
      write_ast stdout sg;
    end;
    close_out stdout
  in
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let initial_env = Compmisc.initial_env () in
  let ast = Pparse.parse_interface ~tool_name ppf sourcefile in
  if !Clflags.dump_parsetree then fprintf ppf "%a@." Printast.interface ast;
  if !Clflags.dump_source then fprintf ppf "%a@." Pprintast.signature ast;

  (* -as-pp *)
  if !Clflags.as_pp then pp_out ast else

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

    (* -as-ppx *)
    if !Clflags.as_ppx then begin
      let sg = Untypeast.untype_signature tsg in
      pp_out sg
    end else

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

let implementation ppf sourcefile outputprefix =
  let pp_out str =
    if !Clflags.pp_text then begin
      Format.printf "%a@." Pprintast.structure str
    end else begin
      let write_ast oc ast =
        output_string oc Config.ast_impl_magic_number;
        output_value oc !Location.input_name;
        output_value oc ast;
      in
      let oc = match !Clflags.output_name with
        | None -> prerr_endline "no output_name!"; stdout
        | Some f -> open_out_bin f
      in
      write_ast oc str;
      close_out oc
    end
  in
  Compmisc.init_path false;
  let modulename = module_of_filename ppf sourcefile outputprefix in
  Env.set_unit_name modulename;
  let env = Compmisc.initial_env() in
  try
    (* -as-pp *)
    if !Clflags.as_pp then begin
      let str = 
        Pparse.parse_implementation ~tool_name ppf sourcefile
        ++ print_if ppf Clflags.dump_parsetree Printast.implementation
        ++ print_if ppf Clflags.dump_source Pprintast.structure
      in
      Warnings.check_fatal ();
      pp_out str
    end else

    let (typedtree, coercion) =
      Pparse.parse_implementation ~tool_name ppf sourcefile
      ++ print_if ppf Clflags.dump_parsetree Printast.implementation
      ++ print_if ppf Clflags.dump_source Pprintast.structure
      ++ Timings.(time (Typing sourcefile))
          (Typemod.type_implementation sourcefile outputprefix modulename env)
      ++ print_if ppf Clflags.dump_typedtree
        Printtyped.implementation_with_coercion
    in
    if !Clflags.print_types then begin
      Warnings.check_fatal ();
      Stypes.dump (Some (outputprefix ^ ".annot"))
    end else 

    (* retype *)      
    let (typedtree, coercion) =
      if !Clflags.no_retype then (typedtree, coercion)
      else begin
        Leopard.without_leopard (fun () ->
            Compmisc.init_path false;
            Env.set_unit_name modulename;
            let env = Compmisc.initial_env() in
            Untypeast.untype_structure typedtree
            ++ Timings.(time (Typing sourcefile))
              (Typemod.type_implementation sourcefile outputprefix modulename env))
      end
    in

    (* -as-ppx *)
    if !Clflags.as_ppx then begin
      Warnings.check_fatal ();
      Stypes.dump (Some (outputprefix ^ ".annot"));
      let str = Untypeast.untype_structure typedtree in
      pp_out str
    end else

    begin
      let bytecode, required_globals =
        (typedtree, coercion)
        ++ Timings.(time (Transl sourcefile))
            (Translmod.transl_implementation modulename)
        ++ Timings.(accumulate_time (Generate sourcefile))
            (fun { Lambda.code = lambda; required_globals } ->
              print_if ppf Clflags.dump_rawlambda Printlambda.lambda lambda
              ++ Simplif.simplify_lambda sourcefile
              ++ print_if ppf Clflags.dump_lambda Printlambda.lambda
              ++ Bytegen.compile_implementation modulename
              ++ print_if ppf Clflags.dump_instr Printinstr.instrlist
              ++ fun bytecode -> bytecode, required_globals)
      in
      let objfile = outputprefix ^ ".cmo" in
      let oc = open_out_bin objfile in
      try
        bytecode
        ++ Timings.(accumulate_time (Generate sourcefile))
            (Emitcode.to_file oc modulename objfile ~required_globals);
        Warnings.check_fatal ();
        close_out oc;
        Stypes.dump (Some (outputprefix ^ ".annot"))
      with x ->
        close_out oc;
        remove_file objfile;
        raise x
    end
  with x ->
    Stypes.dump (Some (outputprefix ^ ".annot"));
    raise x
