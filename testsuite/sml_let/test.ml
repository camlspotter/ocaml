let () =
  let: (* let: is only for local, for now *)
    val x = 1  (* val is mandatory in this branch. 
                  but it can be omitted by indentation rule,
                  once sml_let marries with indent branch.
               *)
    val y = 2

    val x = x + 1   (* x = 2 *)
    and y = x + 1   (* y = 2, not 3 *)

    val () = assert (y = 2)  (* seq_expr is banned. 
                                Always requires val () = e *)

    rec f x = f x   (* recursion *)

    rec f x = g x   (* mutual *)
    and g x = f x

    type t = Foo    (* local type mimiced by 
                       let module TMP = struct type t = Foo end in 
                       let open TMP in
                       ...
                    *)
    val v = Foo
    val () = assert (v = Foo)

    module M = struct
      type t = Bar
    end

    open M

    val v = Bar

    val x =
      let:
        val x = 1
      in
      x
    
  in
  assert (y = 2);
  assert (v = Bar);
  prerr_endline "done"

let () =  (* No val in the toplevel, yet *)
  prerr_endline "byebye"
