open Lexing
open Parser
open Leopardutils

let to_string = function
  | AMPERAMPER             -> "AMPERAMPER"
  | AMPERSAND              -> "AMPERSAND"
  | AND                    -> "AND"
  | AS                     -> "AS"
  | ASSERT                 -> "ASSERT"
  | BACKQUOTE              -> "BACKQUOTE"
  | BANG                   -> "BANG"
  | BAR                    -> "BAR"
  | BARBAR                 -> "BARBAR"
  | BARRBRACKET            -> "BARRBRACKET"
  | BEGIN                  -> "BEGIN"
  | CHAR c                 -> "CHAR " ^ String.make 1 c
  | CLASS                  -> "CLASS"
  | COLON                  -> "COLON"
  | COLONCOLON             -> "COLONCOLON"
  | COLONEQUAL             -> "COLONEQUAL"
  | COLONGREATER           -> "COLONGREATER"
  | COMMA                  -> "COMMA"
  | CONSTRAINT             -> "CONSTRAINT"
  | DO                     -> "DO"
  | DONE                   -> "DONE"
  | DOT                    -> "DOT"
  | DOTDOT                 -> "DOTDOT"
  | DOWNTO                 -> "DOWNTO"
  | ELSE                   -> "ELSE"
  | END                    -> "END"
  | EOF                    -> "EOF"
  | EQUAL                  -> "EQUAL"
  | EXCEPTION              -> "EXCEPTION"
  | EXTERNAL               -> "EXTERNAL"
  | FALSE                  -> "FALSE"
  | FLOAT (s, None)        -> "FLOAT " ^ s
  | FLOAT (s, Some c)      -> "FLOAT " ^ s ^ String.make 1 c
  | FOR                    -> "FOR"
  | FUN                    -> "FUN"
  | FUNCTION               -> "FUNCTION"
  | FUNCTOR                -> "FUNCTOR"
  | GREATER                -> "GREATER"
  | GREATERRBRACE          -> "GREATERRBRACE"
  | GREATERRBRACKET        -> "GREATERRBRACKET"
  | IF                     -> "IF"
  | IN                     -> "IN"
  | INCLUDE                -> "INCLUDE"
  | INFIXOP0 s             -> "INFIXOP0 " ^ s
  | INFIXOP1 s             -> "INFIXOP1 " ^ s
  | INFIXOP2 s             -> "INFIXOP2 " ^ s
  | INFIXOP3 s             -> "INFIXOP3 " ^ s
  | INFIXOP4 s             -> "INFIXOP4 " ^ s
  | INHERIT                -> "INHERIT"
  | INITIALIZER            -> "INITIALIZER"
  | INT (s, None)          -> "INT " ^ s
  | INT (s, Some c)        -> "INT " ^ s ^ String.make 1 c
  | LABEL s                -> "LABEL " ^ s
  | LAZY                   -> "LAZY"
  | LBRACE                 -> "LBRACE"
  | LBRACELESS             -> "LBRACELESS"
  | LBRACKET               -> "LBRACKET"
  | LBRACKETBAR            -> "LBRACKETBAR"
  | LBRACKETLESS           -> "LBRACKETLESS"
  | LBRACKETGREATER        -> "LBRACKETGREATER"
  | LBRACKETPERCENT        -> "LBRACKETPERCENT"
  | LBRACKETPERCENTPERCENT -> "LBRACKETPERCENTPERCENT"
  | LESS                   -> "LESS"
  | LESSMINUS              -> "LESSMINUS"
  | LET                    -> "LET"
  | LIDENT s               -> "LIDENT " ^ s
  | LPAREN                 -> "LPAREN"
  | LBRACKETAT             -> "LBRACKETAT"
  | LBRACKETATAT           -> "LBRACKETATAT"
  | LBRACKETATATAT         -> "LBRACKETATATAT"
  | MATCH                  -> "MATCH"
  | METHOD                 -> "METHOD"
  | MINUS                  -> "MINUS"
  | MINUSDOT               -> "MINUSDOT"
  | MINUSGREATER           -> "MINUSGREATER"
  | MODULE                 -> "MODULE"
  | MUTABLE                -> "MUTABLE"
  | NEW                    -> "NEW"
  | OBJECT                 -> "OBJECT"
  | OF                     -> "OF"
  | OPEN                   -> "OPEN"
  | OPTLABEL s             -> "OPTLABEL " ^ s
  | OR                     -> "OR"
  | PERCENT                -> "PERCENT"
  | PLUS                   -> "PLUS"
  | PLUSDOT                -> "PLUSDOT"
  | PLUSEQ                 -> "PLUSEQ"
  | PREFIXOP s             -> "PREFIXOP " ^ s
  | PRIVATE                -> "PRIVATE"
  | QUESTION               -> "QUESTION"
  | QUOTE                  -> "QUOTE"
  | RBRACE                 -> "RBRACE"
  | RBRACKET               -> "RBRACKET"
  | REC                    -> "REC"
  | RPAREN                 -> "RPAREN"
  | SEMI                   -> "SEMI"
  | SEMISEMI               -> "SEMISEMI"
  | SIG                    -> "SIG"
  | STAR                   -> "STAR"
  | STRING (s, _)          -> "STRING " ^ s
  | STRUCT                 -> "STRUCT"
  | THEN                   -> "THEN"
  | TILDE                  -> "TILDE"
  | TO                     -> "TO"
  | TRUE                   -> "TRUE"
  | TRY                    -> "TRY"
  | TYPE                   -> "TYPE"
  | UIDENT s               -> "UIDENT " ^ s
  | UNDERSCORE             -> "UNDERSCORE"
  | VAL                    -> "VAL"
  | VIRTUAL                -> "VIRTUAL"
  | WHEN                   -> "WHEN"
  | WHILE                  -> "WHILE"
  | WITH                   -> "WITH"
  | COMMENT _              -> "COMMENT"
  | EOL                    -> "EOL"
  | NONREC                 -> "NONREC"
  | HASH                   -> "HASH"
  | HASHOP s               -> "HASHOP " ^ s
  | DOCSTRING _            -> "DOCSTRING .."
  | DOTOP s                -> "DOTOP " ^ s
  | COLONX                 -> "COLONX"
  | COLONPERCENT           -> "COLONPERCENT"
  | COLONPERCENTPERCENT    -> "COLONPERCENTPERCENT"
  | COLONAT                -> "COLONAT"
  | COLONATAT              -> "COLONATAT"
  | COLONATATAT            -> "COLONATATAT"

(** state *)

(** token buffer *)
let queue = Queue.create ()

(** the last token excluding the comments and docstrings *)
let previous_token : [ `Colon of position * token
                     | `None
                     | `Some of token
                     ] ref = ref `None

(** the indent of the current line *)
let indent = ref None

(** special colons stack *)
let stack = ref []

(** debug *)
let debug = try ignore @@ Sys.getenv "OCAMLINDENTDEBUG"; true with _ -> false

let init () = 
  Queue.clear queue;
  previous_token := `None;
  indent := None;
  stack := []

(** Check [p1] and [p2] are in the same file and line *)
let in_the_same_line p1 p2 =
  p1.pos_fname = p2.pos_fname 
  && p1.pos_lnum = p2.pos_lnum

(** Column of the position *)
let column p = p.pos_cnum - p.pos_bol

(* If the token [t] is at a new line, 
   update [indent] then returns its position *)
let update_indent p t = match t with
  | EOL ->
      (* ignore EOL! *)
      None
  | EOF -> 
      (* Make sure EOF flushes all the stack elements *)
      Some { p with pos_cnum = p.pos_bol - 1 } (* so that [column] can return a negavie *)
  | _ -> 
      match !indent with
      | Some p' when in_the_same_line p p' -> None
      | _ -> 
          let i = Some p in
          indent := i;
          i

let rec preprocess lexer lexbuf =
  match Queue.take queue with
  | t -> t
  | exception Queue.Empty ->
      (* Get a new token from the source *)

      let token = lexer lexbuf in
      let start_p = lexeme_start_p lexbuf in

      (* If we see a newline, check the stack and insert closing tokens
         if necessary! *)
      begin match update_indent start_p token with
      | None -> ()
      | Some p -> (* We see a newline *)

          (* Insert implicit closing keywords if necessary *)

          (* Check the opening must be closed before this token *)
          let need_close token p_of_token p_of_opening =
            let i_of_token   = column p_of_token in
            let i_of_opening = column p_of_opening in
            (* BAR is handled specially, since it often appears at the same level of MATCH *)
            if token = BAR then i_of_token < i_of_opening
            else i_of_token <= i_of_opening
          in

          let rec close ever_closed = function
            | [] -> ever_closed, []
            | ((p', t') :: is as stack) ->
                if not (need_close token p p') then
                  ever_closed, stack
                else begin 
                  if debug then 
                    Format.eprintf "Closing %s(%d) by %s(%d)@." 
                      (to_string t')
                      (column p')
                      (to_string token)
                      (column p);
                  (* insert a closing token *)
                  List.iter (flip Queue.add queue)
                    begin match t' with
                    | DO       -> [ COLONX; END; DONE ]
                    | LAZY     -> [ COLONX; END ]
                    | ELSE     -> [ COLONX; END ]
                    | FUNCTION -> [ COLONX; END ]
                    | OBJECT   -> [ COLONX; END ]
                    | SIG      -> [ COLONX; END ]
                    | STRUCT   -> [ COLONX; END ]
                    | THEN     -> [ COLONX; END ]
                    | WITH     -> [ COLONX; END ]
                    | LBRACKETPERCENT
                    | LBRACKETPERCENTPERCENT
                    | LBRACKETAT
                    | LBRACKETATAT
                    | LBRACKETATATAT -> [ RBRACKET ]
                    | _ -> assert false
                    end;
                  close true is (* maybe we have more to close *)
                end
          in
          let closed, stack' = close false !stack in
          stack := stack';

          (* Insert ; 
             If the last token is ;, it is also introduced
             after the closing token.
          *)
          begin match closed, !previous_token with
          | true, `Some SEMI -> Queue.add SEMI queue
          | _ -> ()
          end;
      end;

      (* Insertion of the token itself *)

      let tokens = 
        match !previous_token, token with
        | _, (COMMENT _ | DOCSTRING _) -> [ token ]
        | _, EOL -> [] (* Parser does not understand EOL! *)
  
        (* We need to check the line against colon_pos. This can be different
           from the last token because of COMMENTs and DOCSTRINGs *)

        | _, (COLONPERCENT | COLONPERCENTPERCENT | COLONAT | COLONATAT | COLONATATAT) ->
            (* This is a special colon for attributes or extensions *)
            begin match !indent with
            | None -> assert false (* impossible *)
            | Some p -> 
                let token = match token with
                  | COLONPERCENT        -> LBRACKETPERCENT
                  | COLONPERCENTPERCENT -> LBRACKETPERCENTPERCENT
                  | COLONAT             -> LBRACKETAT
                  | COLONATAT           -> LBRACKETATAT
                  | COLONATATAT         -> LBRACKETATATAT
                  | _ -> assert false
                in
                previous_token := `Colon (p, token);
                stack := (p, token) :: !stack;
                [ token ] 
            end
          
        | `Some t, COLON ->
            begin match t with
            | COMMENT _ -> assert false (* impossible *)
            | DOCSTRING _ -> assert false (* impossible *)

            | THEN
            | ELSE
            | DO
            | FUNCTION
            | OBJECT
            | SIG
            | STRUCT
            | WITH 
            | LAZY ->
                (* This is a special colon! *)
                begin match !indent with
                | None -> assert false (* impossible *)
                | Some p -> 
                    previous_token := `Colon (p, t);
                    stack := (p, t) :: !stack;

                    (* Special insertion.

                       We remove the COLON, but some syntax constructs 
                       require special BEGIN..END instead to help parsing. 
                       
                       For let:, we keep the COLON. 
                    *)
                    match t with
                    | THEN -> [ BEGIN; COLONX ] 
                    | ELSE -> [ BEGIN; COLONX ] 
                    | LAZY -> [ BEGIN; COLONX ] 
                    | DO -> [ BEGIN; COLONX ] 
                    | _ -> [ COLONX ]
                end
  
            | _ -> 
                (* normal colon probably for [e : t] *)
                previous_token := `Some COLON;
                [ COLON ]
            end

        | _ -> 
            previous_token := `Some token;
            [ token ]
      in
      List.iter (fun t -> Queue.add t queue) tokens;

      (* return the first available token back to the parser *)
      match Queue.take queue with
      | q -> q
      | exception Queue.Empty ->
          preprocess lexer lexbuf

let preprocess lexer lexbuf =
  let t = preprocess lexer lexbuf in
  if debug then prerr_endline @@ to_string t;
  t

let token = preprocess Lexer.token
