Line indentation by `///`
====================================

With this patch, you can write line comments in OCaml starting with `///`.

```ocaml
/// this is a comment
```

Incompatibility
-------------------------------------

You can no longer define operators start with `///`.

Bug
-------------------------------------

Operators whose names contain `///` but do not start with `///`
can be still defined and used. This may be confusing:

```ocaml
let (+///) x y = x + y  (* /// is not considered as a comment *)
```
