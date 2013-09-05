==================================
Variant constructors as functions
==================================

Suppose we have:

```ocaml
type t = Foo of int * float
```

Then

```
(Foo) 
```

is equal to `fun (x,y) -> Foo (x,y)`. And,

```
(Foo ..)
```

is equal to `fun x y -> Foo (x,y)`.

It works for list cons operator too:

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

Not very tested well.

Limitations
======================================

P4 does not understand these constructs yet.
