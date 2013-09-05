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
