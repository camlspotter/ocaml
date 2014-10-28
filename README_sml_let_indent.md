+sml_let and +indent
=================================

This document is to explain the new indentation behaviour under 
the conjunction of +sml_let and +indent.

The point is that you can omit writing `val` for single non recursive
`val` bindings inside `let:`. Implicit `val`s are inserted based on
the indentation in `let:` bindings. For example:

```ocaml
let:
   val x = 1
   val y = x
in
...
```

can be written like

```ocaml
let:
   x = 1
   y = x
in
...
```

You cannot omit `val` or `rec` keywords for 
simultaneous non recursive `val` bindings and recursive `rec` bindings:

``ocaml
let:
  val x = 1
  and y = 2

  rec f x = g x
  and g x = f x
in
...
```

To make `let:` indentation rule consistent as the other keywords with `:`,
until `let:`'s cooresponding `in` appears, any lexer tokens cannot appear
left of and the same level of `let:`.

Unlike the other keywords with `:` in +indent, `let:` is not to omit
the corresponding `in`. `in` must appear at the same indent level of `let:`,
otherwise it is rejected as a syntax error:

``ocaml
let:
  val x = 1
  and y = 2

  rec f x = g x
  and g x = f x
 in        <---- Syntax error
...
```

It is possible to make it omittable but I feel `in` should be always written.
 

Implicit `val` insertion: how it works
-------------------------------------------

As the other keywords with `:` sign in +indent, the line must be changed
immediately after `let:`.

If the filter see `let:`, it pushes a new level into the `let:` stack
with the inifinitely large indentation level:

```ocaml
let:
------------->| (* the level is max_int (=infinite) *)
```

At the beginning of each line, if the filter see a token,
it changes its level to the minimum of the current level and 
the indent level of the token:

```ocaml
let:
-------------------->| (* max_int *)
  module ...
  |<------- level
```

If the token is NOT one of to start struct items, then 
insert an implicit `val` before the token:

```ocaml
let:
-------------------->| (* max_int *)
  module ...
  |<------- level
  x = ...  =====> Insert val before x:   val x = ...
  |<------- level
 y = ...   =====> Insert val before y:   val y = ...
 |<------- level 
```

Pros is that the algorithm is very simple.

Cons
----------------------------------

Below, it is clear we must do something before `x`, but `val` is never inserted.
```ocaml
let:
  module M = struct
    ...
  end
  
    x = 1
```

Some design choices
--------------------------------------

* `in` can be also inserted automatically based on the indentation, but I do not like it. Haskell does not have it outside of `do`.