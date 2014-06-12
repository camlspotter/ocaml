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

Polymorphic variants as functions
---------------------------------------------

```ocaml
(`Foo ..)
```

is equivalent to 

```ocaml
fun x -> `Foo x
```

Note that the polymorphic variant constructors can take at most 
one argument and it is determined purely syntactically. 
Therefore `(\`Foo ..)` can take only one argument:

```ocaml
`(`Foo..) (1,2,3)  (* `Foo (1,2,3) *)
`(`Foo..) 1 2 3    (* (`Foo 1) 2 3  which ends in a type error *)
```

Code `(\`Foo)` has no special meaning. It is just equivalent to `\`Foo`.

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
