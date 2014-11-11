let () =
  let: (* let: is only for local, for now *)
    x = 1
    y = 2

    val x = x + 1   (* x = 2 *)
    and y = x + 1   (* y = 2, not 3 *)

    () = assert (y = 2)  (* seq_expr is banned. 
                                Always requires val () = e 
                                
                                It is since:
                                * I do not force writing ;;
                                * I want to omit val 
                             *)

    rec f x = f x   (* recursion *)

    rec f x = g x   (* mutual *)
    and g x = f x

    type t = Foo    (* local type mimiced by 
                       let module TMP = struct type t = Foo end in 
                       let open TMP in
                       ...
                    *)
    v = Foo
    val () = assert (v = Foo) (* you can still write val *)

    module M = struct
      type t = Bar
    end

    open M

   v = Bar

   x =
      let:
        x = 1
      in
      x
    
  in
  assert (y = 2);
  assert (v = Bar);
  prerr_endline "done"

let () =  (* No val in the toplevel, yet *)
  prerr_endline "byebye"
