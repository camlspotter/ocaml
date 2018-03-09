open Leopardutils
open Leopardparsing
open Ast_mapper
open Longident
open Parsetree
open Location

let leopard_mode () = match !Clflags.leopard_mode with
  | Some true -> true
  | _ -> false

let overload_vdesc vdesc = 
  { vdesc with pval_prim = ["%OVERLOADED"] } (* All capital by a historical reason *)

(*
let imp_vdesc vdesc = { vdesc with pval_prim = ["%imp"] }
 *)

let rename lid n = match lid with
  | Lident m -> "__" ^ String.lowercase_ascii m ^ "_" ^ n
  | Ldot (Lident "Bigarray", m) -> "__bigarray_" ^ String.lowercase_ascii m ^ "_" ^ n
  | _ -> assert false
  
(* XXX Defined also in btype.ml *)
let is_implicit = function 
  | Asttypes.Labelled s -> String.length s > 0 && s.[0] = '_' 
  | _ -> false

let rewrite_imp vdesc = match vdesc.pval_type.ptyp_desc with
  | Ptyp_arrow (l, t1, t2) when is_implicit l ->
     (* val %imp n : _d:t1 -> t2  =>  extern n : _d:t1 -> t2 Leopard.Implicit.alias = "%identity" *)
     let loc = t2.ptyp_loc (*XXX ghost *) in
     let lid = Longident.(Ldot(Ldot(Lident "Leopard", "Implicits"),"alias")) in
     let alias = { ptyp_desc= Ptyp_constr ({txt=lid; loc}, [t2])
                 ; ptyp_loc= loc (* XXX ghost *)
                 ; ptyp_attributes = []
                 }
     in
     { vdesc
       with pval_type= { vdesc.pval_type with ptyp_desc= Ptyp_arrow (l, t1, alias) }
          ; pval_prim= ["%identity"]
     }
(*
  | Ptyp_arrow (_l, _t1, _t2) ->
     (* val %imp n : t  =>  extern n : t = "%imp" *)
     imp_vdesc vdesc
 *)
  | _ -> assert false (* error XXX *)

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
       { i with pstr_desc = Pstr_primitive (rewrite_imp vdesc) }
    | _ -> super.structure_item self i
  in
  let signature_item self i = match i.psig_desc with
    (* val %overload n : t  =>  extern n : t = "%OVERLOADED" *)
    | Psig_extension ( ({txt="overload"},
                        PSig [{psig_desc= Psig_value vdesc}]),
                       _ ) ->
        { i with psig_desc = Psig_value (overload_vdesc vdesc) }

    | Psig_extension ( ({txt="imp"},
                        PSig [{psig_desc= Psig_value vdesc}]),
                       _ ) ->
       { i with psig_desc = Psig_value (rewrite_imp vdesc) }
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
    let expr self e = match e.pexp_desc with
      (* We leave `open %imp M` *)
      | Pexp_extension ({ txt = "imp" }, PStr [ { pstr_desc = Pstr_eval ({ pexp_desc= Pexp_open _ }, _)} ] ) -> super.expr self e

      | Pexp_extension ({txt="imp"; loc}, pld) ->
          (* [%imp spec]  =>  (Leopard.Implicit.get : _d:(_, <spec>) Leopard.Implicits.t -> _) [@imp_omitted] : *)
          let loc = Location.ghost loc in
          let leopard_implicits x = { txt= Longident.(Ldot(Ldot(Lident "Leopard","Implicits"),x)); loc } in
          let mkty ptyp_desc = { ptyp_desc; ptyp_loc= loc; ptyp_attributes= [] } in
          let spec = !from_payload_to_core_type_forward loc pld in
          { e with pexp_desc= Pexp_constraint( { e with pexp_desc= Pexp_ident (leopard_implicits "get")
                                                      ; pexp_attributes = [] },
                                               mkty (Ptyp_arrow (Asttypes.Labelled "_d", 
                                                                 mkty (Ptyp_constr ( leopard_implicits "t",
                                                                                     [ mkty Ptyp_any; spec ] )),
                                                                 mkty Ptyp_any)))
          }
      | _ -> super.expr self e
        in
        { super with typ; expr }
end

let make_mapper () = Imp.extend & extend default_mapper
let structure s = let mapper = make_mapper () in mapper.structure mapper s
let signature s = let mapper = make_mapper () in mapper.signature mapper s
