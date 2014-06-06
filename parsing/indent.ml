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
  | CHAR _ -> "CHAR of (char)"
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
  | FLOAT _ -> "FLOAT of (string)"
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
  | INFIXOP0 _ -> "INFIXOP0 of (string)"
  | INFIXOP1 _ -> "INFIXOP1 of (string)"
  | INFIXOP2 _ -> "INFIXOP2 of (string)"
  | INFIXOP3 _ -> "INFIXOP3 of (string)"
  | INFIXOP4 _ -> "INFIXOP4 of (string)"
  | INHERIT -> "INHERIT"
  | INITIALIZER -> "INITIALIZER"
  | INT _ -> "INT of (int)"
  | INT32 _ -> "INT32 of (int32)"
  | INT64 _ -> "INT64 of (int64)"
  | LABEL _ -> "LABEL of (string)"
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
  | LIDENT _ -> "LIDENT of (string)"
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
  | NATIVEINT _ -> "NATIVEINT of (nativeint)"
  | NEW -> "NEW"
  | OBJECT -> "OBJECT"
  | OF -> "OF"
  | OPEN -> "OPEN"
  | OPTLABEL _ -> "OPTLABEL of (string)"
  | OR -> "OR"
  | PERCENT -> "PERCENT"
  | PLUS -> "PLUS"
  | PLUSDOT -> "PLUSDOT"
  | PLUSEQ -> "PLUSEQ"
  | PREFIXOP _ -> "PREFIXOP of (string)"
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
  | STRING _ -> "STRING of (string * string option)"
  | STRUCT -> "STRUCT"
  | THEN -> "THEN"
  | TILDE -> "TILDE"
  | TO -> "TO"
  | TRUE -> "TRUE"
  | TRY -> "TRY"
  | TYPE -> "TYPE"
  | UIDENT _ -> "UIDENT of (string)"
  | UNDERSCORE -> "UNDERSCORE"
  | VAL -> "VAL"
  | VIRTUAL -> "VIRTUAL"
  | WHEN -> "WHEN"
  | WHILE -> "WHILE"
  | WITH -> "WITH"
  | COMMENT _ -> "COMMENT of (string * Location.t)"
  | EOL -> "EOL"


(** state *)

(** token buffer *)
let queue = Queue.create ()

(** the last token excluding the comments *)
let previous_token : [ `Colon of position * token
                     | `None
                     | `Some of token ] ref = ref `None

(** the indent of the current line *)
let indent = ref None

(** special colons stack *)
let stack = ref []

let init () = 
  Queue.clear queue;
  previous_token := `None;
  indent := None;
  stack := []

let in_the_same_line p1 p2 =
  p1.pos_fname = p2.pos_fname && p1.pos_lnum = p2.pos_lnum

let update_indent p t =
  match !indent with
  | Some (p', _) when in_the_same_line p p' -> None
  | _ -> 
      let i = Some (p, t) in
      indent := i;
      i

let indentation p = p.pos_cnum - p.pos_bol

let rec preprocess lexer lexbuf =
  match Queue.take queue with
  | t -> t
  | exception Queue.Empty ->

      let token = lexer lexbuf in
      let start_p = lexeme_start_p lexbuf in

      (* If we see a newline, check the stack and insert closing tokens
         if necessary! *)
      begin match update_indent start_p token with
      | None -> ()
      | Some (p, t) ->
          let rec close = function
            | [] -> []
            | ((p', t') :: is as stack) ->
                if 
                  if t' = BAR then indentation p' < indentation p
                  else indentation p' <= indentation p
                then begin 
                  Queue.add 
                    begin match t' with
                    | DO -> DONE
                    | ELSE -> END
                    | FUNCTION -> END
                    | OBJECT -> END
                    | SIG -> END
                    | STRUCT -> END
                    | THEN -> END
                    | WITH -> END
                    (* | LET | REC *)
                    | _ -> assert false
                    end queue;
                  close is
                end else stack
          in
          stack := close !stack
      end;

      let token = 
        match !previous_token, token with
        | _, COMMENT _ -> Some token
        | _, EOL -> None (* Oh, if preprocessor is here, the original creates EOL! *)
  
        | `None, _ -> 
            previous_token := `Some token;
            Some token 
  
        (* We need to check the line against colon_pos. This can be different
           from the last token because of COMMENTs *)
        | `Colon (colon_pos, _key), _ when in_the_same_line colon_pos start_p ->
            (* error. After special : you must change a line *)
            assert false
  
        | `Colon _, _ -> 
            previous_token := `Some token;
            Some token 
  
        | `Some t, COLON ->
            begin match t with
            | COMMENT _ -> assert false
                
            | DO
            | ELSE
            | FUNCTION
  (*
            | LET
  *)
            | OBJECT
  (*
            | REC
  *)
            | SIG
            | STRUCT
            | THEN
            | WITH ->
                (* special colon *)
                begin match !indent with
                | Some (p, _) -> 
                    previous_token := `Colon (p, t);
                    stack := (p, t) :: !stack;
                    begin match t with
                    | ELSE | FUNCTION | THEN | WITH -> Some BEGIN
                    | _ -> None
                    end
                | None -> assert false
                end
  
            | _ -> 
                previous_token := `Some COLON;
                Some COLON
            end

        | _ -> 
            previous_token := `Some token;
            Some token
      in
      begin match token with
      | Some t -> Queue.add t queue 
      | None -> ()
      end;

      match Queue.take queue with
      | q -> q
      | exception Queue.Empty ->
          preprocess lexer lexbuf

(*
let preprocess lexer lexbuf =
  let t = preprocess lexer lexbuf in
  prerr_endline @@ to_string t;
  t
*)
