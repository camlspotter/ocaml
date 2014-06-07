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
                     | `Some of token ] ref = ref `None

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
  | EOF -> 
      (* Make sure EOF flushes all the stack elements *)
      Some ( { p with pos_cnum = p.pos_bol - 1 }, (* negative indent! *)
             t )
  | _ -> 
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
      let end_p =   lexeme_end_p   lexbuf in

      (* If we see a newline, check the stack and insert closing tokens
         if necessary! *)
      begin match update_indent start_p token with
      | None -> ()
      | Some (p, t) ->
          let rec close ever_closed = function
            | [] -> ever_closed, []
            | ((p', t') :: is as stack) ->
                if 
                  if t = BAR then indentation p < indentation p'
                  else indentation p <= indentation p'
                then begin 
                  if debug then 
                    Format.eprintf "Closing %d %d@." 
                      (indentation p)
                      (indentation p');
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
                  close true is
                end else ever_closed, stack
          in
          let closed, stack' = close false !stack in
          stack := stack';
          (* Insert ; 
             If the last token is ;, it is also introduced
             after the closing token.
          *)
          match closed, !previous_token with
          | true, `Some SEMI -> Queue.add SEMI queue
          | _ -> ()
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
            raise Syntaxerr.(Error (Expecting ({ Location.loc_start= start_p;
                                                 loc_end= end_p;
                                                 loc_ghost = false },
                                               "newline")))
  
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

let preprocess lexer lexbuf =
  let t = preprocess lexer lexbuf in
  if debug then prerr_endline @@ to_string t;
  t
