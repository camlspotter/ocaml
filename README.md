Polymorphic record in OCaml
=====================================

This ppx adds polymorphic record.

`[%poly_record <exp>]`
-------------------------------------

In side the extension `[%poly_record ...]`, 
the record syntax is are changed from the normal (monomorphic) records
to polymorphic records whose type is `_ Ppx_poly_record.Poly_record.t`.

Record creation `{ l = e; .. }`
--------------------------------------

```ocaml
# [%poly_record { x = 1; y = 1.0 }];;
- : < x : int; y : float > Ppx_poly_record.Poly_record.t = <abstr>
```

Unlike the normal monomorphic records, it is not required to declare
fields of the polymorphic records. They are inferred using OCaml's
object type. 

Field access `r.l`
--------------------------------------

Accessing fields of the polymorphic records is by `r.x` inside
`[%poly_record ..]`:

```ocaml
# [%poly_record fun r -> r.x];;
- : < x : 'tvar_1; .. > Ppx_poly_record.Poly_record.t -> 'tvar_1 = <fun>
```

Record copy with field updates: `{ r with l = e; .. }`
-----------------------------------------------------------

The syntax of record copy `{ r with x = e }` works for polymorphic records too:

```ocaml
# [%poly_record fun r -> { r with x = 1 }];;
- : (< x : int; .. > as 'a) Ppx_poly_record.Poly_record.t 
    -> 'a Ppx_poly_record.Poly_record.t
```

Field mutation `r.l <- e`
--------------------------------

Field mutation `r.x <- e` works in `[%poly_record ..]`, too, but a bit differently from the monomorphic record, since the polymorphic record has no type declaration to specify a field is mutable. In `[%poly_record ..]`, `r.x <- e` works when the field `x` is a reference:

```ocaml
# [%poly_record let r = { x = ref 0 } in r.x <- 1; r];;
- : < x : int ref; .. > Ppx_poly_record.Poly_record.t = <abstr>
```

`[%mono_record ..]`
---------------------------------

You can use `[%mono_record ..]` inside `[%poly_record ..]` to use
the original monomorphic records.

Todo
============

* Other extensions for other parts than expressions: `[%%poly_record ..]`
* let [%poly_record] = true in ... or something equivalent
* Patterns: `{ x = p }`. This is diffcult and probably requires ppx_pattern_guard

+poly_record
====================

ocaml+poly_record is an integration of ppx_poly_record into the OCaml compiler internal.

Including all the features above, ocaml+poly_record supports the following special syntax for polymorphic records:

```ocaml
{. x = 1; y = 2 }   (* creation *)
{. e with x = 1 }   (* update *)
r..l                (* access *)
r..l <- 2           (* modification *)
```
