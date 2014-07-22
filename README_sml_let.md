+sml_let
=================================

`+sml_let` provides Standard ML style `let`. SML `let` is to introduce
a local context: it is not restricted only for value bindings 
but all the other structure items.

The new `let:` keyword is introduced for SML let. The original `let` is
kept as is for the compatibility.

Value bindings
-----------------------------

In `let:`, values are bound by `val` or `rec`.

    let:
      val x = 1   (* keyword val is required *)
     
      val x = 2
      and y = x   (* y = 1, not 2. *)
      
      rec f x = g x
      and g x = f x
    in
    ...

This is equivalent with the following original code:

    let x = 1
    in 

    let x = 2
    and y = x   (* y = 1, not 2. *)
    in  

    let rec f x = g x
    and g x = f x
    in
    ...

Local types, exceptions and modules
--------------------------------------------

The other structure items like types, exceptions and modules
can be declared locally inside `let:`

    let:

      type t = Foo

      module M = struct let x = 1 end

      open M

      let y = x
    in
    ...

Thus with `let:`, you can declare local types, exceptions and modules
more like you are in the module toplevel context.

The above code is desugarred using local modules of the original syntax:

    let module _tmp_ = struct

      type t = Foo

      module M = struct let x = 1 end

      open M

      let y = x
    in
    let open _tmp_ in
    ...

Therefore type errors happened around `let:` may have strange module
identifier `_tmp_`. To understand the error messages, 
you should simply skip the ident. 


Test
------------------------------

Test code is available at `testsuite/sml_let/test.ml`.
