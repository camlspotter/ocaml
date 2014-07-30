η-expansion sugar
===============================

+eta introduces one char syntax sugar to introduce η-expansions (eta-expansions).

Motivation
-------------------------------

OCaml has the value polymorphism. It is relaxed but many function applications
still cannot be typed with a polymorphic type without the help of 
*η-expansion*:

```ocaml
# let id x = x;;
val id : 'a -> 'a = <fun>

# let id2 = id id;;
val id2 : '_a -> '_a = <fun>    (* not polymorphic due to the value restriction *)

# let id3 = fun x -> id id x    (* η-expansion recovers the polymorphism *)
val id3 : 'a -> 'a = <fun>
```

Adding an η-expansion to expression `e` is trivial 
but the code needs to be modified in two places,
before and after of the expression: `e` => `fun x -> (e) x`.
This requires some cursor movement and is cumbersome when `e` is huge
and occupies many lines.

You can reduce the place of modifications into one using the following trick:

```ocaml
let id4 = fun x -> x |> id id   (* modification required only in front of id id *)
```

But this does not work well when the expression contains other binary operators
with the same connectivity power. 
You have to move the cursor to add parentheses around the expression:

```ocaml
let ($) f g x = f (g x);;
let id5 = id $ id                    (* '_a -> '_a *)
let not_id = fun x -> x |> id $ id   (* This is not id! The type is ('a -> 'b) -> 'a -> 'b *)
let id6 = fun x -> x |> (id $ id)    (* Valid, but you need parens. *)
```

`&` for η-expansion.
-------------------------------------------

+eta's syntax sugar resolves this problem. You can introduce 
an η-expansion just insert `&` after `=` of a let binding or 
a method declaration:

```ocaml
# let id7 = & id id      (* This is same as let id7 = fun x -> (id id) x *)
val id7 : 'a -> 'a = <fun>
```

More generally,

* `let p = & e` is expanded to `let p x = (e) x` 
* `method p = & e` is expanded to `method p x = (e) x`

where `x`'s are fresh variables.

How to remember
---------------------------------------------

The symbol `&` is from Latin cursive of the word "et",
which is the best ASCII character to denotes "eta".
