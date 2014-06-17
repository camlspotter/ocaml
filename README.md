Haskellish value : type declaration
======================================

With this patch, you can write Haskell like value-type declaration
in let bindings:

```ocaml
let f : 'a . 'a -> 'a 
and f x = x
in
...
```

This is equivalent to the following:

```ocaml
let f : 'a . 'a -> 'a = fun x -> g x
in
...
```

`let x : t and ...` simply replaces the occurrence of the pattern variable
`x` with `x : t` in `...`. Therefore the following is not valid:

```ocaml
let f : 'a . 'a -> 'a 
and (f,g) = (fun x -> x), (fun x -> x)
;;
```

since it is equivalent to the following invalid code:

```ocaml
let (f : 'a . 'a -> 'a ,g) = (fun x -> x), (fun x -> x)
;;
```

Explicitly quantified type annotation like `'a . 'a -> 'a` is only
permitted with a sole pattern variable like `let f : 'a . 'a -> 'a = ...`.
