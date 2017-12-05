let () =
  if true then begin
    if true then:
        prerr_endline "hello";
      end (* It was used for closing then: *)
    (* The inserted end here was used for the outer begin! *)
;;
