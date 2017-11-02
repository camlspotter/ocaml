open Leopardutils
 open Ast_mapper
open Longident
open Parsetree
open Location

let leopard_mode () = match !Clflags.leopard_mode with
  | Some true -> true
  | _ -> false

let overload_vdesc vdesc = 
  { vdesc with pval_prim = ["%OVERLOADED"] } (* All capital by a historical reason *)

let imp_vdesc vdesc = { vdesc with pval_prim = ["%imp"] }

let rename lid n = match lid with
  | Lident m -> "__" ^ String.lowercase_ascii m ^ "_" ^ n
  | Ldot (Lident "Bigarray", m) -> "__bigarray_" ^ String.lowercase_ascii m ^ "_" ^ n
  | _ -> assert false
  
  
let extend super =
  let expr = if not @@ leopard_mode () then super.expr else
      fun self e -> match e.pexp_desc with
        | Pexp_apply( ({ pexp_loc;
                         pexp_desc = Pexp_ident { txt= Ldot (m, f); loc= loc } } as fn), 
                      args ) when pexp_loc.loc_ghost && loc.loc_ghost -> 
            begin match m, f with
            | Lident ("Array" | "String"),
              ("get" | "set" | "unsafe_get" | "unsafe_set")
            | Ldot (Lident "Bigarray", ("Genarray" | "Array1" | "Array2" | "Array3")),
              ("get" | "set" | "unsafe_get" | "unsafe_set") -> 
                let fn' = { fn with pexp_desc= Pexp_ident { txt= Lident (rename m f); loc } } in
                { e with pexp_desc = Pexp_apply ( fn', args ) }
            | _ -> super.expr self e
            end
        | Pexp_ident { txt= Ldot (m, f); loc= loc }  when e.pexp_loc.loc_ghost && loc.loc_ghost->
            begin match m, f with
            | Lident ("Array" | "String"),
              ("get" | "set" | "unsafe_get" | "unsafe_set")
            | Ldot (Lident "Bigarray", ("Genarray" | "Array1" | "Array2" | "Array3")),
              ("get" | "set" | "unsafe_get" | "unsafe_set") -> 
                { e with pexp_desc= Pexp_ident { txt= Lident (rename m f); loc } }
            | _ -> super.expr self e
            end
        | _ -> super.expr self e
  in
  let structure_item self i = match i.pstr_desc with
    (* val %overload n : t  =>  extern n : t = "%OVERLOADED" *)
    | Pstr_extension ( ({txt="overload"},
                        PStr [{pstr_desc= Pstr_primitive vdesc}]),
                       _ ) ->
        { i with pstr_desc = Pstr_primitive (overload_vdesc vdesc) }
    (* val %imp n : t  =>  extern n : t = "%imp" *)
    | Pstr_extension ( ({txt="imp"},
                        PStr [{pstr_desc= Pstr_primitive vdesc}]),
                       _ ) ->
        { i with pstr_desc = Pstr_primitive (imp_vdesc vdesc) }
    | _ -> super.structure_item self i
  in
  let signature_item self i = match i.psig_desc with
    (* val %overload n : t  =>  extern n : t = "%OVERLOADED" *)
    | Psig_extension ( ({txt="overload"},
                        PSig [{psig_desc= Psig_value vdesc}]),
                       _ ) ->
        { i with psig_desc = Psig_value (overload_vdesc vdesc) }
    (* val %imp n : t  =>  extern n : t = "%imp" *)
    | Psig_extension ( ({txt="imp"},
                        PSig [{psig_desc= Psig_value vdesc}]),
                       _ ) ->
        { i with psig_desc = Psig_value (imp_vdesc vdesc) }
    | _ -> super.signature_item self i
  in
  { super with expr; structure_item; signature_item }

module Imp = struct
  let from_payload_to_core_type_forward = ref (fun _ -> assert false : Location.t -> payload -> core_type)

  (*
  
    Pre-preprocessing for syntax sugars for 
      [%imp <spec>]
  *)
  
  let extend super =
    let typ self cty =
      match cty.ptyp_desc with
      | Ptyp_extension ({txt="imp"; loc}, pld) -> !from_payload_to_core_type_forward loc pld
      | _ -> super.typ self cty
    in
    { super with typ }
end

let make_mapper () = Imp.extend & extend default_mapper
let structure s = let mapper = make_mapper () in mapper.structure mapper s
let signature s = let mapper = make_mapper () in mapper.signature mapper s
