indent : OCaml + indentation rule
====================================

This small patch provides Python like indentation rules to OCaml:

* 100% backward compatible
* Indentation rule is only introduced by Python like special keywords: `with:`, `then:`, `else:`, `do:`, `function:` etc.

Example
====================================

Special keywords ends with `:` introduces implicit blocks automatically
based on the code indentation.

The following program using two special keywords `then:` and `else:`:

    let f e =
      if e then:
        print_endline "true!";
        42
      else:
        print_endline "false!";
        -1
    in
    ...

is equivalent to the original OCaml code below:

    let f e = 
      if e then begin              (* <- begin inserted *)
        print_endline "true!";
        42
      end else begin               (* <- end and begin inserted *)
        print_endline "false!";
        -1
      end                          (* <- end inserted *)  
    in
    ...


This Python style `xxx:` keywords are introduced all the paired keywords
like `do` and `done`, `object` and `end`, etc. and all the open keywords
without the corresponding closings such as `match .. with`, `try .. with`,
`funciton`.

`do:` does not require the paired `done`:

    for i = 1 to 100 do:
      print_int i;
    print_endline "printed 100"      (* lowering indent implicitly closes do: *)

is equilvalent with

    for i = 1 to 100 do
      print_int i;
    done;
    print_endline "printed 100"

`with:` introduces implicit `begin` and `end`, useful in nested matches:

    match xs with
    | [] ->
      match y with:    (* introduces implicit begin *)
      | A -> 2
      | B -> 3
    | x::xs ->         (* lowering indent implicitly closes the begin *)
      match y with:    (* introduces implicit begin *)
      | A -> 4
      | B -> 5         (* EOF implicitly closes the begin *)
                       
is equlivalent with  

    match xs with
    | [] ->
      begin match y with
      | A -> 2
      | B -> 3
      end
    | x::xs ->
      begin match y with
      | A -> 4
      | B -> 5
      end

Special keywords
====================================

Auto-insertion of `begin` .. `end` for the following keywords:

* `with:`, `then:`, `else:`, `function:` and `lazy:`

Auto-insertion of the corresponding ending keywords for the following:

* `do:` (`done` is not required)
* `sig:`, `struct:`, `object:` (`end` is not required)

When the implicit closings are inserted?
===========================================

The implicit closing of a special keyword `xxx:` happens when 
the indentation level goes to *less than* or *equal to* 
the indentation level where the special keyword is introduced:

    for i = 0 to 100 do:         (* indent level 0 *)
      print_int i;               (* level 2 *)
    print_endline "printed 100!" (* back to 0 *)

is equivalent to

    for i = 0 to 100 do
      print_int i;
    done;
    print_endline "printed 100!"

Note that this is not the horizontal level of the special keyword:

    match x with
      p -> function:   (* indent level 2 at p. Not 7 at function: *)
        | A -> 1       (* level 4. No closing happens *) 
        | B -> 2
    | q -> ...         (* Level 0. *)

is equvalient with

    match x with
      p -> begin function
        | A -> 1
        | B -> 2
        end
    | q -> ...

Indentation of the line starts with `|`
------------------------------------------

The indentation level of the line which starts with the vertical bar `|` 
for the pattern matches is treated a bit differently, in order to support
the common indentation convention of or-patterns: they are often leveled
at the same with `match`, `funciton` and `try`. 

At the lines starts with `|`, the auto closing only happens when their
indentation levels are *strictly less than* those of the lines with 
the corresponding special keywords:

    let rec f x = 
      match x mod 3, x mod 5 with:    (* level 2. Introduces an implicit begin. *)
      | 0, 0 -> print_string "fizbuz" (* level 2. This does not close the implicit begin *)
      | 0, _ -> print_string "fiz"    (* level 2. *)
      | _, 0 -> print_string "buz"    (* level 2. *)
      | _ -> print_int x;             (* level 2. *)
      f (x+1)                         (* level 2. Start not with | *)

is equivalent to

    let rec f x = 
      begin match x mod 3, x mod 5 with
      | 0, 0 -> print_string "fizbuz"
      | 0, _ -> print_string "fiz"
      | _, 0 -> print_string "buz"
      | _ -> print_int x;
      end;
      f (x+1)

Handling of `;`
------------------------------------------

If `;` symbol for sequential execution `e1; e2` appears at the end of a line
and implicit closing happens just after this line, then the `;` is treated
as if it is after the closing:

    for i = 0 to 100 do:
      print_int i;                   (* ; appears at the end of do: indentation block *)
    print_endline "printed 100!"
            
is equilvalent with

    for i = 0 to 100 do
      print_int i;
    done;                            (* ; is used to sequence the next line *)
    print_endline "printed 100!"
            
Note
====================================

`xxx:` must be at the end of lines
-------------------------------------

After the special keywords, you must immediately change the line:

    match e1 with: p -> e2

is rejected as a syntax error. You can still write comments:

    match e1 with: (* special keyword! *)
    | p -> e2

is ok.

Attributes
-------------------------------------

Special `xxx:` keywords whose original versions can take attributes
are also able to take attributes, after `:` signs changing the line:

    function:
    [@blahblah]
    | p -> e

The line changing is mandatory. `function: [@blahblah]` may look better 
but it is not possible for the current implementation approach 
as a simple lexer level converter.

Behind the scene
====================================

This hack uses 'Lexer preprocessor', which is introduced in OCaml 4.02.0
in `lexer.mll`. There is a function `Lexer.set_preprocessor` to push 
your own lexer level filter between the lex token stream and the parser.
There is no way to use this from the command line, and you need to modify
the compiler but it seems easy to use. (Except that the preprocessor receives
`EOL` tokens at each end of line, unlike the original lex token streams.)

To make the hack around implicit `begin` introduction as simple as possible,
some syntaxes are extended. For example, the following is valid with this patch:

      match x mod 3, x mod 5 with begin (* begin after with *)
      | 0, 0 -> print_string "fizbuz"
      | 0, _ -> print_string "fiz"
      | _, 0 -> print_string "buz"
      | _ -> print_int x;
      end

This variant is actually used for the desugared version of `match ... with:`.
Technically, for lexer based filter, it is hard to insert `begin` in front 
of `match` when it sees `with:` in its token stream.

Future work?
====================================

* More tests

* `let:` (`let: rec` or `let rec:` ?)

     Is it really required? `in`-less `let` in Haskell's `do` 
     has no serious meaning to me.

* `begin:` and `(:`

     Uh, oh.
