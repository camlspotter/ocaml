open Lexing
open Parser

let to_string = function
  | AMPERAMPER -> "AMPERAMPER"
  | AMPERSAND -> "AMPERSAND"
  | AND -> "AND"
  | AS -> "AS"
  | ASSERT -> "ASSERT"
  | BACKQUOTE -> "BACKQUOTE"
  | BANG -> "BANG"
  | BAR -> "BAR"
  | BARBAR -> "BARBAR"
  | BARRBRACKET -> "BARRBRACKET"
  | BEGIN -> "BEGIN"
  | CHAR c -> "CHAR " ^ Printf.sprintf "%c" c
  | CLASS -> "CLASS"
  | COLON -> "COLON"
  | COLONCOLON -> "COLONCOLON"
  | COLONEQUAL -> "COLONEQUAL"
  | COLONGREATER -> "COLONGREATER"
  | COMMA -> "COMMA"
  | CONSTRAINT -> "CONSTRAINT"
  | DO -> "DO"
  | DONE -> "DONE"
  | DOT -> "DOT"
  | DOTDOT -> "DOTDOT"
  | DOWNTO -> "DOWNTO"
  | ELSE -> "ELSE"
  | END -> "END"
  | EOF -> "EOF"
  | EQUAL -> "EQUAL"
  | EXCEPTION -> "EXCEPTION"
  | EXTERNAL -> "EXTERNAL"
  | FALSE -> "FALSE"
  | FLOAT s -> "FLOAT " ^ s
  | FOR -> "FOR"
  | FUN -> "FUN"
  | FUNCTION -> "FUNCTION"
  | FUNCTOR -> "FUNCTOR"
  | GREATER -> "GREATER"
  | GREATERRBRACE -> "GREATERRBRACE"
  | GREATERRBRACKET -> "GREATERRBRACKET"
  | IF -> "IF"
  | IN -> "IN"
  | INCLUDE -> "INCLUDE"
  | INFIXOP0 s -> "INFIXOP0 " ^ s
  | INFIXOP1 s -> "INFIXOP1 " ^ s
  | INFIXOP2 s -> "INFIXOP2 " ^ s
  | INFIXOP3 s -> "INFIXOP3 " ^ s
  | INFIXOP4 s -> "INFIXOP4 " ^ s
  | INHERIT -> "INHERIT"
  | INITIALIZER -> "INITIALIZER"
  | INT _ -> "INT of (int)"
  | INT32 i -> "INT32 " ^ Int32.to_string i
  | INT64 i -> "INT64 " ^ Int64.to_string i
  | LABEL s -> "LABEL " ^ s
  | LAZY -> "LAZY"
  | LBRACE -> "LBRACE"
  | LBRACELESS -> "LBRACELESS"
  | LBRACKET -> "LBRACKET"
  | LBRACKETBAR -> "LBRACKETBAR"
  | LBRACKETLESS -> "LBRACKETLESS"
  | LBRACKETGREATER -> "LBRACKETGREATER"
  | LBRACKETPERCENT -> "LBRACKETPERCENT"
  | LBRACKETPERCENTPERCENT -> "LBRACKETPERCENTPERCENT"
  | LESS -> "LESS"
  | LESSMINUS -> "LESSMINUS"
  | LET -> "LET"
  | LIDENT s -> "LIDENT " ^ s
  | LPAREN -> "LPAREN"
  | LBRACKETAT -> "LBRACKETAT"
  | LBRACKETATAT -> "LBRACKETATAT"
  | LBRACKETATATAT -> "LBRACKETATATAT"
  | MATCH -> "MATCH"
  | METHOD -> "METHOD"
  | MINUS -> "MINUS"
  | MINUSDOT -> "MINUSDOT"
  | MINUSGREATER -> "MINUSGREATER"
  | MODULE -> "MODULE"
  | MUTABLE -> "MUTABLE"
  | NATIVEINT v -> "NATIVEINT " ^ Nativeint.to_string v
  | NEW -> "NEW"
  | OBJECT -> "OBJECT"
  | OF -> "OF"
  | OPEN -> "OPEN"
  | OPTLABEL s -> "OPTLABEL " ^ s
  | OR -> "OR"
  | PERCENT -> "PERCENT"
  | PLUS -> "PLUS"
  | PLUSDOT -> "PLUSDOT"
  | PLUSEQ -> "PLUSEQ"
  | PREFIXOP s -> "PREFIXOP " ^ s
  | PRIVATE -> "PRIVATE"
  | QUESTION -> "QUESTION"
  | QUOTE -> "QUOTE"
  | RBRACE -> "RBRACE"
  | RBRACKET -> "RBRACKET"
  | REC -> "REC"
  | RPAREN -> "RPAREN"
  | SEMI -> "SEMI"
  | SEMISEMI -> "SEMISEMI"
  | SHARP -> "SHARP"
  | SIG -> "SIG"
  | STAR -> "STAR"
  | STRING (s, _) -> "STRING " ^ s
  | STRUCT -> "STRUCT"
  | THEN -> "THEN"
  | TILDE -> "TILDE"
  | TO -> "TO"
  | TRUE -> "TRUE"
  | TRY -> "TRY"
  | TYPE -> "TYPE"
  | UIDENT s -> "UIDENT " ^ s
  | UNDERSCORE -> "UNDERSCORE"
  | VAL -> "VAL"
  | VIRTUAL -> "VIRTUAL"
  | WHEN -> "WHEN"
  | WHILE -> "WHILE"
  | WITH -> "WITH"
  | COMMENT _ -> "COMMENT"
  | EOL -> "EOL"


(** state *)

(** token buffer *)
let queue = Queue.create ()

(** the last token excluding the comments *)
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
  
let in_the_same_line p1 p2 =
  p1.pos_fname = p2.pos_fname && p1.pos_lnum = p2.pos_lnum

let update_indent p t =
  match t with
  | EOL ->
      (* ignore EOL! *)
      None
  | EOF -> 
      (* Make sure EOF flushes all the stack elements *)
      Some { p with pos_cnum = p.pos_bol - 1 } (* negative indent! *)
  | _ -> 
      match !indent with
      | Some p' when in_the_same_line p p' -> None
      | _ -> 
          let i = Some p in
          indent := i;
          i

let indentation p = p.pos_cnum - p.pos_bol

let rec preprocess lexer lexbuf =
  match Queue.take queue with
  | t -> t
  | exception Queue.Empty ->

      let token = lexer lexbuf in
      let start_p = lexeme_start_p lexbuf in
      let end_p =   lexeme_end_p   lexbuf in

      (* If we see a newline, check the stack and insert closing tokens
         if necessary! *)
      begin match update_indent start_p token with
      | None -> ()
      | Some p -> (* We see a newline *)

          (* Insert implicit closing keywords *)

          let rec close ever_closed = function
            | [] -> ever_closed, []
            | ((p', t', _) :: is as stack) ->
                if 
                  (if token = BAR then (<) else (<=))
                    (indentation p)
                    (indentation p')
                then begin 
                  if debug then 
                    Format.eprintf "Closing %s(%d) by %s(%d)@." 
                      (to_string t')
                      (indentation p')
                      (to_string token)
                      (indentation p);
                  Queue.add
                    begin match t' with
                    | DO       -> DONE
                    | ELSE     -> END
                    | FUNCTION -> END
                    | OBJECT   -> END
                    | SIG      -> END
                    | STRUCT   -> END
                    | THEN     -> END
                    | WITH     -> END
                    | LAZY     -> DONE
                    | LET      -> END (* let: begin <bindings> end in *)
                    | _ -> assert false
                    end queue;
                  close true is
                end else ever_closed, stack
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

          (* val insertion *)
    
          (* val insertion occurs when the token can start a let pattern. *)
          let can_start_let_pattern = match token with
            | LIDENT _
            | LPAREN
            | LAZY
            (* | EXCEPTION *) (* let exception Failure s = failwith "err" in s  is syntax error *)
            | UNDERSCORE
            | SHARP
            | LBRACE
            | LBRACKET
            | LBRACKETBAR
            | MINUS
            | PLUS
            | INT _
            | CHAR _
            | STRING _
            | FLOAT _ 
            | INT32 _
            | INT64 _
            | NATIVEINT _
            | LBRACKETPERCENT
            | FALSE
            | TRUE
            | UIDENT _
            | BACKQUOTE -> true
            | _ -> false
          in
    
          begin match !stack with
          | (p, LET, posref) :: _ when indentation start_p <= !posref ->
                (* We are now in the following line with 'trigger'.
        
                   let:
                     module M = struct  <- trigger
                       ...
                     end                <- trigger
                     x =                <- trigger
                   in                   <- trigger
                *)
                if can_start_let_pattern then begin
                  if debug then
                    Format.eprintf "Line with %s at let: %d@."
                      (to_string token)
                      !posref;
                  Queue.add VAL queue
                end;
                posref := indentation start_p;
            | _ -> ()
          end;
      end;

      (* Insertion of the token itself *)

      let tokens = 
        match !previous_token, token with
        | _, COMMENT _ -> [ token ]
        | _, EOL -> 
            [] (* Oh, if preprocessor is here, the original creates EOL! *)
  
        | `None, _ -> 
            previous_token := `Some token;
            [ token ]
  
        (* We need to check the line against colon_pos. This can be different
           from the last token because of COMMENTs *)
        | `Colon (colon_pos, _key), _ when in_the_same_line colon_pos start_p ->
            (* error. After special : you must change a line *)
            raise Syntaxerr.(Error (Expecting ({ Location.loc_start= start_p;
                                                 loc_end= end_p;
                                                 loc_ghost = false },
                                               "newline")))
  
        | `Colon _, _ -> 
            previous_token := `Some token;
            [ token ]
  
        | `Some t, COLON ->
            begin match t with
            | COMMENT _ -> assert false
                
            | DO
            | ELSE
            | FUNCTION
            | LET
            | OBJECT
            | SIG
            | STRUCT
            | THEN
            | WITH 
            | LAZY ->
                (* special colon *)
                begin match !indent with
                | Some p -> 
                    previous_token := `Colon (p, t);
                    (* the ref is only for LET... *)
                    stack := (p, t, ref max_int) :: !stack;

                    (* Special insertion.

                       We remove the COLON, but some syntax constructs 
                       require special BEGIN..END instead to help parsing. 
                       
                       For let:, we also keep the COLON. *)
                    begin match t with
                    | ELSE | FUNCTION | THEN | WITH -> [ BEGIN ]
                    | LET -> [ COLON; BEGIN ] (* let: begin <bindings> end in *) 
                    | LAZY -> 
                        (* lazy: need to be handled a bit differently,
                           since 
                             lazy: [@x] e
                           should not be translated to
                             lazy begin [@x] e end
                           which is a valid expression in OCaml and is not
                           equivalent with
                             lazy [@x] begin e end
                        *)   
                        [ DO ]
                    | _ -> []
                    end
                | None -> assert false
                end
  
            | _ -> 
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
