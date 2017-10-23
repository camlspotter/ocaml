open Ast_mapper
open Longident
open Parsetree
open Location

let with_leopardlib = ref false
    
let overload_vdesc vdesc = 
  { vdesc with pval_prim = ["%OVERLOADED"] }

let rec prefix lid = function
  | Lident s -> Ldot (lid, s)
  | Ldot (t1, s) -> Ldot (prefix lid t1, s)
  | Lapply _ -> assert false
  
let extend super =
  let expr = if not !with_leopardlib then super.expr else
      fun self e -> match e.pexp_desc with
        | Pexp_apply( ({ pexp_loc;
                         pexp_desc = Pexp_ident { txt= (Ldot (m, f) as lid); loc= loc } } as fn), 
                      args ) when pexp_loc.loc_ghost && loc.loc_ghost -> 
            begin match m, f with
            | (Lident ("Array" | "String") | Ldot (Lident "Bigarray", ("Genarray" | "Array1" | "Array2" | "Array3"))), ("get" | "set" | "unsafe_get" | "unsafe_set") -> 
                let fn' = { fn with pexp_desc= Pexp_ident { txt= prefix (Ldot (Lident "Leopard", "DotBracket")) lid; loc } } in
                { e with pexp_desc = Pexp_apply ( fn', args ) }
            | _ -> super.expr self e
            end
        | _ -> super.expr self e
  in
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
  { super with expr; structure_item; signature_item }

let make_mapper () = extend default_mapper
let structure s = let mapper = make_mapper () in mapper.structure mapper s
let signature s = let mapper = make_mapper () in mapper.signature mapper s
