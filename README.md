Variant constructors as functions
==================================

Suppose we have:

```ocaml
type t = Foo of int * float
```

Then

```ocaml
(Foo) 
```

is equal to `fun (x,y) -> Foo (x,y)`. And,

```ocaml
(Foo ..)
```

is equal to `fun x y -> Foo (x,y)`.

It works for list cons operator too:

```ocaml
(::) : ('a * 'a list) -> 'a list
(:: ..) : 'a -> 'a list -> 'a list
```

Record fields and class methods as functions
---------------------------------------------

This patch also provides the following new syntax construct:

Record fields as functions:

```ocaml
(.label)
```

is equvalient to 

```ocaml
fun x -> x.label
```

Class methods as functions:

```ocaml
(#m)
```

is equivalent with

```ocaml
fun x -> x#m
```

Samples
---------------------------------------------

You can try examples at `testsuite/curried-constr/test.ml`.
