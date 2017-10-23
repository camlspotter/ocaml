open Asttypes
open Ast_mapper
open Parsetree

let with_leopardlib = ref false
    
let overload_vdesc vdesc = 
  { vdesc with pval_prim = ["%OVERLOADED"] }
  
let extend super =
  let structure_item self i = match i.pstr_desc with
    (* val %overload n : t  =>  extern n : t = "%OVERLOADED" *)
    | Pstr_extension ( ({txt="overload"},
                        PStr [{pstr_desc= Pstr_primitive vdesc}]),
                       _ ) ->
        prerr_endline "hoK";
        { i with pstr_desc = Pstr_primitive (overload_vdesc vdesc) }
    | _ -> super.structure_item self i
  in
  let signature_item self i = match i.psig_desc with
    (* val %overload n : t  =>  extern n : t = "%OVERLOADED" *)
    | Psig_extension ( ({txt="overload"},
                        PStr [{pstr_desc= Pstr_primitive vdesc}]),
                       _ ) ->
        { i with psig_desc = Psig_value (overload_vdesc vdesc) }
    | _ -> super.signature_item self i
  in
  { super with structure_item; signature_item }

let mapper = extend default_mapper
let structure = mapper.structure mapper
let signature = mapper.signature mapper
