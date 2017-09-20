val debug : bool
(** Env var OCAMLINDENTDEBUG to turn this variable [true] *)
  
val init : unit -> unit
val token : Lexing.lexbuf -> Parser.token
