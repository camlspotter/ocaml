Haskellish `value : type` declaration
======================================

In +type_at_let, you can write Haskell like value-type declaration
in let bindings:

```ocaml
let f : 'a . 'a -> 'a 
and f x = x
in
...
```

or without its syntax extension,

```ocaml
let f : 'a . 'a -> 'a = [%val]
and f x = x
in
...
```

This is equivalent to the following:

```ocaml
let f : 'a . 'a -> 'a = fun x -> x
in
...
```

which makes some people feel lousy, since adding the polymorphic type
to `let f x = x` requires lots of key types and cursor moves.

What it does
-------------------------------

`let x : t and ...` simply replaces the occurrence of the pattern variable
`x` with `x : t` in `...`. 

Therefore the following is not valid:

```ocaml
let f : 'a . 'a -> 'a 
and (f,g) = (fun x -> x), (fun x -> x)
;;
```

since it is equivalent to the following invalid code:

```ocaml
let ((f : 'a . 'a -> 'a), g) = (fun x -> x), (fun x -> x)
;;
```

Explicitly quantified type constraint like `'a . 'a -> 'a` is only
permitted with a sole pattern variable like `let f : 'a . 'a -> 'a = ...`.

Note
-------------------------------

Note that this is a pure syntax sugar provides Haskellish look of 
value-type annotations, but the meaning of OCaml's type *constraints* 
is not changed at all. Unquantified type variables can be instantiated
just like the normal OCaml type constraints:

```ocaml
let f : 'a . 'a -> 'b -> 'a 
and f x y = print_int y; x
```

In the above, the inferred type of `f` is not `'a . 'a -> 'b -> 'a`
but `'a . 'a -> int -> 'a`.

Example
-------------------------

You can find some examples in `testsuite/type_at_let/test.ml`.
