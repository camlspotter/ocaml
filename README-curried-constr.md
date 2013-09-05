==================================
Variant constructors as functions
==================================

This is a small OCaml compiler modification where you can use variant constructors as
functions without writing their arguments explicitly.

Suppose we have:

```ocaml
type t = Foo of int * float
```

Then

```ocaml
(Foo)          (* Parens are mandatory *)
```

is equal to `fun (x,y) -> Foo (x,y)`. And,

```ocaml
(Foo ..)       (* Parens are mandatory *)
```

is equal to `fun x y -> Foo (x,y)`. So, for example,

```ocaml
let () =
  assert (List.map (Foo) [(1,2.0); (3,4.0)] = [Foo (1,2.0); Foo (3,4.0)]);
  assert (List.map (fun f -> f 3.0) (List.map (Foo..) [1;2]) = [Foo (1,3.0); Foo (2,3.0)])
```

It works for the list cons operator too:

```
(::) : ('a * 'a list) -> 'a list
(:: ..) : 'a -> 'a list -> 'a list
```

How to build
======================================

`make world` does not work yet since P4 is not modified at all.

```bash
$ ./configure
$ make core coreboot ocaml
```

How to play
======================================

```bash
$ ./runocaml
```

Quality
======================================

Not very well tested, but this is trivial source conversion slightly depending on type info.

Limitations
======================================

P4 does not understand these constructs yet.
