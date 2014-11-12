open Asttypes
open Ast_helper
open Ppxx (* must come after Ast_helper *)
open Ast_mapper
open Parsetree
open Longident
open Location

(* This is a copy of Btype.hash_variant. Btype is not available yet... *)
let hash s =
  let accu = ref 0 in
  for i = 0 to String.length s - 1 do
    accu := 223 * !accu + Char.code s.[i]
  done;
  (* reduce to 31 bits *)
  accu := !accu land (1 lsl 31 - 1);
  (* make it signed for 64 bits architectures *)
  if !accu > 0x3FFFFFFF then !accu - (1 lsl 31) else !accu

let poly_record = ref []

let with_poly_record b f =
  poly_record := b :: !poly_record;
  let res = f () in
  poly_record := List.tl !poly_record;
  res

let is_poly_record () = match !poly_record with
  | [] -> false
  | b::_ -> b

let poly_record ?loc x = lid ?loc & "PolyRecord." ^ x

let error_field_with_module loc =
  ppx_errorf ~loc "Polymorphic record fields cannot take module names"

let check_attrs_empty loc = function
  | [] -> ()
  | _ -> ppx_errorf ~loc "This extension cannot have structure item attributes [@@...]"

let check_fields fs =
  let uniq_dup_by eq fs = 
    let rec loop firsts dups = function
      | [] -> List.rev firsts, List.rev dups
      | x::xs -> 
          let firsts', dups' = 
            match List.find (eq x) firsts with
            | found -> firsts, (x,found)::dups 
            | exception Not_found -> x::firsts, dups
          in
          loop firsts' dups' xs
    in
    loop [] [] fs
  in
  let _firsts, dups = uniq_dup_by (fun (_,_,l,_,_) (_,_,l',_,_) -> l = l') fs in
  begin match dups with
  | [] -> ()
  | ((_,_,l,_,e),_) :: _ -> ppx_errorf ~loc:e.pexp_loc "Duplicated label %s" l
  end;
  let _firsts, dups = uniq_dup_by (fun (_,_,_,h,_) (_,_,_,h',_) -> h = h') fs in
  begin match dups with
  | [] -> ()
  | ((_,_,l,_,e),(_,_,l',_,_)) :: _ ->
      ppx_errorf ~loc:e.pexp_loc "Label %s cannot coexist with %s sharing the same hash" l l'
  end

let extend super =
  let expr self e = match e.pexp_desc with
    | Pexp_extension ({txt="poly_record"}, PStr [ { pstr_desc= Pstr_eval (e, attrs) } ]) ->
        check_attrs_empty e.pexp_loc attrs;
        with_poly_record true & fun () -> self.expr self e
    | Pexp_extension ({txt="mono_record"}, PStr [ { pstr_desc= Pstr_eval (e, attrs) } ]) ->
        check_attrs_empty e.pexp_loc attrs;
        with_poly_record false & fun () -> self.expr self e
    | Pexp_extension ({txt=("poly_record" | "mono_record")}, _) ->
        ppx_errorf ~loc:e.pexp_loc "This extension must take only one expression"
    | _ when not (is_poly_record ()) -> super.expr self e

    (* is_poly_record () *)

    | Pexp_record (fields, eopt) ->
        (*
          { l1 = v1; l2 = v2 }

          (* binds *)
          let v1 = $e1 in
          let v2 = $e2 in

          (* enforce *)
          if false then 
            ignore (fun (o : 'a) -> 
              ignore (o#$l1 = v1);
              ignore (o#$l2 = v2)
            );
          
          (Poly_record.copy_with [| ($l1_hash, Obj.repr $v1); ... |] (None | Some (e : 'a Poly_record.t)) : 'a Poly_record.t)
        *)
        let open Exp in
        with_default_loc (Location.ghost e.pexp_loc) & fun () ->
          let fields = List.mapi (fun i ({txt; loc}, e) ->
            match txt with
            | Longident.Lident l -> 
                let eloc = Location.ghost e.pexp_loc in
                let loc = Location.ghost loc in
                let name = "v" ^ string_of_int i in
                (Pat.var ~loc:eloc name,
                 Exp.var ~loc:eloc name,
                 l, 
                 int ~loc (hash l), 
                 e)
            | _ -> error_field_with_module loc) fields
          in
          check_fields fields;
          let binds e =
            let_ Nonrecursive 
              (List.map (fun (p, _, _, _, e) -> Vb.mk p e) fields)
              e
          in
          let tvar = Typ.new_var "poly_record" in
          let tvar_poly_record_t = Typ.constr (poly_record "t") [tvar] in
          let enforce =
            (* if false then 
                 (fun (o : 'a) -> 
                   ignore (o#$l1 = v1);
                   ignore (o#$l2 = v2)
                 ) (assert false);
            *)
            ifthenelse
              (bool false) 
              (let ov,op = ExpPat.var "o" in
               apply
                 (fun_ "" None 
                    (Pat.(constraint_ op tvar))
                    (seqs 
                     & List.map (fun (_, v, l, _, _) -> 
                       ignore_
                       & apply (id "=")
                         [ "", send ov l 
                         ; "", v ])
                       fields))
                 ["", assert_false ()]
              )
              None
          in
          let array = 
            array ~loc:(Location.ghost e.pexp_loc) ~attrs:e.pexp_attributes
            & List.map (fun (_,_,_,hash,e) -> 
              tuple ~loc:(Location.ghost hash.pexp_loc)
                [hash; 
                 apply (id "Obj.repr") ["",e]]) fields 
          in
          let eopt' = match eopt with
            | None -> None
            | Some e -> Some (constraint_ e tvar_poly_record_t)
          in
          let copy_with =
            constraint_
              (apply (ident (poly_record "copy_with"))
                 [ "", array
                 ; "", option eopt' ])
              tvar_poly_record_t
          in
          binds (sequence enforce copy_with)

    | Pexp_field (e', {txt=Lident l; loc=loc'}) ->
        (* 
           (Poly_record.get ($e' : < $l : 'a; ..> Poly_record.t) $l_hash : 'a)
        *)
        let open Exp in
        with_default_loc (Location.ghost e.pexp_loc) & fun () ->
          let loc' = Location.ghost loc' in
          let hash = int ~loc:loc' (hash l) in
          let tvar = Typ.new_var "poly_record" in
          let obj_poly_record_t =
            Typ.(constr (poly_record "t") [ object_ [l, [], tvar] Open ])
          in
          constraint_
            (apply (ident (poly_record "get"))
               [ "", constraint_ e' obj_poly_record_t
               ; "", hash])
            tvar
    | Pexp_field (_, {txt=_lident; loc=f_loc}) ->
        (* [x.M.l] is error *)
        error_field_with_module f_loc
                  
    | Pexp_setfield (e', {txt=Lident l; loc=loc'}, e'') ->
        (* 
           Poly_record.set ($e' : < $l : 'a ref; ..> Poly_record.t) $l_hash (e'' : 'a)
        *)
        let open Exp in
        with_default_loc (Location.ghost e.pexp_loc) & fun () ->
          let loc' = Location.ghost loc' in
          let hash = int ~loc:loc' (hash l) in
          let tvar = Typ.new_var "poly_record" in
          let tvar_ref = Typ.ref_ tvar in
          let obj_poly_record_t =
            Typ.(constr (poly_record "t") [ object_ [l, [], tvar_ref] Open ])
          in
          apply (ident (poly_record "set"))
            [ "", constraint_ e' obj_poly_record_t
            ; "", hash
            ; "", constraint_ e'' tvar
            ]
    | Pexp_setfield (_, {txt=_lident; loc=f_loc}, _) ->
        (* [x.M.l <- e'] is error *)
        error_field_with_module f_loc
                  
    | _ -> super.expr self e
  in
  { super with expr }

let mapper = extend default_mapper

let () = Ast_mapper.extend_builtin_mapper extend
