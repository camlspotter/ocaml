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

is equivalent with the original OCaml code below:

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

* `with:`, `then:`, `else:` and `function:`

Auto-insertion of the corresponding ending keywords for the following:

* `do:` (`done` is not required)
* `sig:`, `struct:`, `object:` (`end` is not required)

When the implicit closeings are inserted?
===========================================

The implicit closing happens when the indentation level goes 
to less than or equal to the indentation level where the special keywords 
are introduced:

    for i = 0 to 100 do:
      print_int i;
    print_endline "printed 100!"

is equivalent with

    for i = 0 to 100 do
      print_int i;
    done;
    print_endline "printed 100!"

Note that this is not the horizontal level of the special keywords:

    match x with
      p -> function:   
        | A -> 1       (* No closing happens *) 
        | B -> 2
    | q -> ...         (* Closing since the level is less than the line of function: *)

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
the common indentation convention of or-patterns. Auto closing only happens 
for these lines when their indentation levels are strictly less than 
the indentation levels of the lines with special keywords:

    let rec f x = 
      match x mod 3, x mod 5 with:   (* Introduces an implicit begin *)
      | true, false -> print_string "fiz" (* This does not close the implicit begin *)
      | false, true -> print_string "buz"
      | true,  true -> print_string "fizbuz"
      | _ -> print_int x;
      f (x+1)                        (* close is inserted before this line *)

is equivalent with

    let rec f x = 
      begin match x mod 3, x mod 5 with
      | true, false -> print_string "fiz"
      | false, true -> print_string "buz"
      | true,  true -> print_string "fizbuz"
      | _ -> print_int x;
      end;
      f (x+1)

Handling of `;`
------------------------------------------

If `;` symbol for sequential execution `e1; e2` appears at the end of a line
and this line is considered to be implicitly closed, then the `;` is treated
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

After the special keywords, you must immediately change the line:

    match e1 with: p -> e2

is rejected as a syntax error. You can still write comments:

    match e1 with: (* special keyword! *)
    | p -> e2

is ok.


Future work?
====================================

* `let:` (`let: rec` or `let rec:` ?), but is it really required?
* `lazy:`
* Auto closing of `;;` using indentation levels
* More tests
