Field accessor as functions
==================================

Record fields and class methods as functions
---------------------------------------------

This patch provides the following syntax sugar:

Record fields as functions:

```ocaml
(.label)       (* This is not valid in the vanilla OCaml *)
(!).label      (* If you like to keep within the vanilla *)
```

are equvalient to 

```ocaml
fun x -> x.label
```

Record field modifications as functions:

```ocaml
(.label) <- e       (* This is not valid in the vanilla OCaml *)
(!).label <- e
```

are equvalient to 

```ocaml
fun x -> x.label <- e
```

Full record field modifications as functions:

```ocaml
(.label<-)       (* This is not valid in the vanilla OCaml *)
(!<-).label
```

are equvalient to 

```ocaml
fun x e -> x.label <- e
```

Class methods as functions:

```ocaml
(#m)                 (* This is not valid in the vanilla OCaml *)
(!)#m
```

are equivalent with

```ocaml
fun x -> x#m
```

Samples
---------------------------------------------

You can try examples at `testsuite/fields/test.ml`.
