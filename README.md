indent : OCaml + indentation rule
====================================

This small patch provides Python like indentation rules to OCaml:

* 100% backward compatible
* Indentation rule is only introduced by Python like special keywords: `with:`, `then:`, `else:`, `do:`, `function:` etc.

Example
====================================

In the Original OCaml syntax:

    let f x y =
      match x with
      | 1 ->
        begin match y with  (* You need begin *)
        | true -> 2
        | false -> 3 
        end                 (* You need end *)
      | 2 -> 4
      | _ -> 100

The above code can be written now like:

    let f x y =
      match x with
      | 1 ->
        match y with:       (* with: to turn on the indentation rule *)
        | true -> 2
        | false -> 3        (* No need of closing by end *)
      | 2 -> 4
      | _ -> 100

Special keywords
====================================

Auto-insertion of `begin` .. `end` for the following keywords:

* `with:`
* `then:` and `else:`
* `function:`

Auto-insertion of the corresponding ending keywords for the following:

* `do:` (`done` is not required)
* `sig`, `struct`, `object` (`end` is not required)

Note
====================================

After the special keywords, you must immediately change the line:

    match e with: 1 -> 2

is rejected. You can still write comments:

    match e with: (* special keyword! *)
    | 1 -> ...

is ok.
