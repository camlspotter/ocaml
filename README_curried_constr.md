Variant constructors as functions
==================================

Suppose we have:

```ocaml
type t = Foo of int * float
```

Then

```ocaml
Foo
```

is equal to `fun (x,y) -> Foo (x,y)`. And,

```ocaml
(Foo ..)        (* This is not valid in the vanilla OCaml *)
!Foo
```

is equal to `fun x y -> Foo (x,y)`.

It works for list cons constructor too:

```ocaml
(::) : ('a * 'a list) -> 'a list
(:: ..) : 'a -> 'a list -> 'a list  (* This is not valid in the vanilla OCaml *)
!(::) : 'a -> 'a list -> 'a list
```

Polymorphic variants as functions
---------------------------------------------

```ocaml
(`Foo ..)         (* This is not valid in the vanilla OCaml *)
!`Foo
```

is equivalent to 

```ocaml
fun x -> `Foo x
```

Note that ``(`Foo ..)`` can take only one argument:
the arity of the polymorphic variant constructors is at most one
and  it is determined purely syntactically. 


```ocaml
(`Foo..) (1,2,3)  (* `Foo (1,2,3) *)
(`Foo..) 1 2 3    (* (`Foo 1) 2 3  which ends in a type error *)
```

Code ``(`Foo)`` has no special meaning. It is just equivalent to `` `Foo``.

Record fields and class methods as functions
---------------------------------------------

This patch also provides the following new syntax construct:

Record fields as functions:

```ocaml
(.label)       (* This is not valid in the vanilla OCaml *)
(!).label
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

is equivalent with

```ocaml
fun x -> x#m
```

Samples
---------------------------------------------

You can try examples at `testsuite/curried-constr/test.ml`.
