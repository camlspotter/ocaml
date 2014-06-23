Pattern guards in OCaml
=====================================

This patch adds (*pattern guards*)[http://citeseer.ist.psu.edu/erwig00pattern.html] to OCaml. This is inspired by (ocaml-patterns)[http://code.google.com/p/ocaml-patterns/wiki/PatternGuards], an old CamlP4 extension for pattern guards, but is an independent implementation.

Pattern guard `with p <- e`
-------------------------------------

Pattern guard is an extension of OCaml *guard* (or *boolean guard*) `when e` 
in `match`, `function` and `try` cases.
(Do not be confused with *guards* (*boolean guards*) and *pattern guards*.)

While `when e` only takes a boolean expression `e` to have an additional test
to a case, `with p <- e` can take a pattern match: if `e`'s value matches 
with the pattern `p`, the case is selected, and the variables 
in the pattern `p` are bound in the case action, the right hand side of `->`.
If the value does not match, the case is skipped.

Pattern guards and boolean guards can be sequenced.
They are tested in their appearence order. The bound variables 
in pattern guards can be used in the later pattern guards and boolean guards,
in addition to the case action.

Thread safety
-------------------------------------

Pattern guards are thread-safe; its desugaring uses no mutabile data.

Performance
-------------------------------------

Any expression construct with pattern guards has slight overhead
compared to one without pattern guards.

You can grasp it to see how expressions with pattern guards are desugared.
For example the following expression with a pattern guard:

```ocaml
match e with
| p with p' <- e' -> e''
```

is desugared into the following vanilla OCaml program:

```ocaml
let module PG = struct exception WithExit of int end in
try
  match e with
  | p when 
        (match e' with
         | p' -> raise (PG.WithExit (Obj.magic e'' : 'a))
         | _ -> false)
       -> assert false
with
| PG.WithExit v -> (Obj.magic v : 'a)
```

Using a reference might have better performance but it would sacrifice the thread safety. The best solution is to implement pattern guards not as a syntax sugar but directly in the pattern match compiler.

Test sample
---------------------------

Available at `testsuite/pattern_guards/test.ml`.
