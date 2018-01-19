type 'a show = ('a -> string, [%imp just Show]) Leopard.Implicits.t

val %imp show : ?d:'a show -> 'a -> string
  
module Show = struct
  (* Skip this candidate because of non decreasing %imp recursive dependency:
     <= The message is only printed in the debug. We should output it as a warning.
  *) 
  let broken : ?d:'a list show -> 'a -> string = fun ?d x -> show ?d [x]
end

let () = assert (show 1 = "1")
