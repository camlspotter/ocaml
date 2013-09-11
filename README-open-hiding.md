================================================
Exclusion of specific names from module open
================================================

This is a small OCaml compiler modification which enables excluding
specified names of opening modules. 

This may be useful if you get Warning 44: "this open statement shadows ... (which is later used)" of OCaml 4.01.0. To remove the warning you can explicitly retrict names opened by `open M`.

This is inspired Haskell's `import M hiding (...)`.

Syntax
================================

`open M` is extended and now you can write:

```ocaml
open {M - { a; b; c }}
```

`open {M - { a; b; c }}` is as same as `open M` but `M.a`, `M.b` and `M.c` are not 
opened into the current typing environment as `a`, `b` and `c` respectively.
This does not mean that these names are completely hidden from the module;
you can still access them by `M.a`, `M.b` and `M.c`.

If you write identifiers in the exclusion list then anything with the same name 
will be excluded from the opening: `open {M - { a }}` excludes all the value,
type, exception, class and class type named `a` if they exist.
For fine tuning, you can specify the kind of the name:

* `val x`
* `type t`
* `exception e`
* `class c`
* `class type ct`
* `module M`
* `module type MT`

for example, you can write `open {M - { a; type t }}` to exclude anything with
the name `a` and type `t`.

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

There is an example file hiding_test.ml .

Bug
======================================

Variant constructors and record labels are only excluded from opening 
only when the types they belong to are excluded.
There is no fine tuning of excluding them individually.

You can write any identifiers in the exclusion list for now. 
There is no check performed to see they really exist in the module.

I am not proud of the syntax. Technically `open M-{ ... }` is just enough.

Limitations
======================================

P4 does not understand these constructs yet.
(P4 is the biggest show-stopper for compiler modification.)
