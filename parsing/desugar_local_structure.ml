open Asttypes
open Longident
open Parsetree

let rec structure exp rev_str = 
  let flush e = match rev_str with
    | [] -> e
    | rev_ss ->
        (* let module TMP = struct ... end in
           let open TMP in
           e    
        *)   
        let tmp = { txt = "_tmp_"; loc = Location.none } in
        let tmp_l = { txt = Lident "_tmp_"; loc = Location.none } in
        { pexp_desc = 
            Pexp_letmodule(tmp, 
                           { pmod_desc= Pmod_structure (List.rev rev_str);
                             pmod_loc = Location.none;
                             pmod_attributes = [] },
                           { pexp_desc = Pexp_open (Fresh, tmp_l, e);
                             pexp_loc = Location.none;
                             pexp_attributes = [] });
          pexp_loc = Location.none;
          pexp_attributes = [] }
  in
  function
  | [] -> flush exp
  | s::ss ->
      let e_ss = structure exp rev_str ss in
      match s.pstr_desc with
      | Pstr_eval (e, attr) ->
          (* e;; ss => e; ss *)
          flush { pexp_desc = Pexp_sequence (e, e_ss);
                  pexp_loc = s.pstr_loc;
                  pexp_attributes = attr (* ? *) }
      | Pstr_value (rf, vbs) ->
          flush { pexp_desc = Pexp_let (rf, vbs, e_ss);
                  pexp_loc = s.pstr_loc; (* ? *)
                  pexp_attributes = [] }
      | Pstr_module mb -> 
          flush { pexp_desc = Pexp_letmodule (mb.pmb_name, mb.pmb_expr, e_ss);
                  pexp_loc = mb.pmb_loc; (* ? *)
                  pexp_attributes = mb.pmb_attributes (* ? *)
          }
      | Pstr_open od ->
          flush { pexp_desc = Pexp_open (od.popen_override, od.popen_lid, e_ss);
                  pexp_loc = od.popen_loc;
                  pexp_attributes = od.popen_attributes (* ? *)
                }
      | _ ->
          structure exp (s::rev_str) ss           

let desugar str exp = structure exp [] str

