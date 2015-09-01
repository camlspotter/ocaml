# Variant constructors as functions

Suppose we have:

```ocaml
type t = Foo of int * float
```

Then

```ocaml
Foo
```

is equal to `fun (x,y) -> Foo (x,y)`. And,

```
!Foo
```

is equal to `fun x y -> Foo (x,y)`.

# Polymorphic variants as functions

```ocaml
!`Foo
```

is equivalent to 

```ocaml
fun x -> `Foo x
```

Note that ``!`Foo`` always take only one argument:
the arity of the polymorphic variant constructors is at most one
and  it is determined purely syntactically. 


```ocaml
!`Foo (1,2,3)  (* `Foo (1,2,3) *)
!`Foo 1 2 3    (* (`Foo 1) 2 3  which ends in a type error *)
```

Code ``(`Foo)`` has no special meaning. It is just equivalent to `` `Foo``.

# Samples

You can try examples at `testsuite/curried_constr/test.ml`.

# Limitations

## Cons constructor

Cons constructor `(::)` is specially handled in OCaml
and it is outside of the support of `ppx_curried_constr`.
