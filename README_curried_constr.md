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
(Foo ..)
[%fun] Foo
```

is equal to `fun x y -> Foo (x,y)`.

It works for list cons constructor too:

```ocaml
(::) : ('a * 'a list) -> 'a list
(:: ..) : 'a -> 'a list -> 'a list
[%fun] (::) : 'a -> 'a list -> 'a list
```

Polymorphic variants as functions
---------------------------------------------

```ocaml
(`Foo ..)
[%fun] `Foo
```

is equivalent to 

```ocaml
fun x -> `Foo x
```

Note that ``(`Foo ..)`` can take only one argument:
the arity of the polymorphic variant constructors is at most one
and  it is determined purely syntactically. 


```ocaml
`(`Foo..) (1,2,3)  (* `Foo (1,2,3) *)
`(`Foo..) 1 2 3    (* (`Foo 1) 2 3  which ends in a type error *)
```

Code ``(`Foo)`` has no special meaning. It is just equivalent to `` `Foo``.

Record fields and class methods as functions
---------------------------------------------

This patch also provides the following new syntax construct:

Record fields as functions:

```ocaml
(.label)
[%fun].label
```

is equvalient to 

```ocaml
fun x -> x.label
```

Class methods as functions:

```ocaml
(#m)
[%fun]#m
```

is equivalent with

```ocaml
fun x -> x#m
```

Samples
---------------------------------------------

You can try examples at `testsuite/curried-constr/test.ml`.
