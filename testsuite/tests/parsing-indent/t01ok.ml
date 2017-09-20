(* begin end insertion *)

(* match with: *)
let rec fizbuz x = 
  match x mod 3, x mod 5 with:
  | 0, 0 -> print_string "fizbuz"
  | 0, _ -> print_string "fiz"
  | _, 0 -> print_string "buz"
  | _ -> print_int x;
  fizbuz (x+1)

(* try with: *)
let f g x =
  match g with
  | Some g ->
      try 
        g x 
      with:
      | Not_found -> x
  | None -> x

(* if then: else: *)
let f e =
  if e then:
    print_endline "true!";
    42
  else:
    print_endline "false!";
    -1 (* You cannot write ;; here *)

(* function: *)

let f = function
  | Some x ->
      function:
      | Some y -> x + y
      | None -> x
  | None -> fun _ -> 0

(* lazy: *)
let f e =
  if true then lazy:
    prerr_endline "true";
    1 + e
  else:
    print_endline "else";
    lazy:
      prerr_endline "false";
      e * 2

(* for .. do: *)
let f n =
  for i = 0 to n do:
    print_int i;
  print_string "loop done";

  for i = 0 to n do:
    print_int i
  ; print_string "loop done" (* semi is here *)

(* while .. do: *)
let f p n =
  while p n do:
    prerr_endline "loop!";
    incr n;
  prerr_endline "loop end"

(* sig: and struct: *)

module M : sig:
  val x : int
= struct:
  let x = 1

(* object: *)

class c = object:
  method x = 1

class type ct = object:
  method x : int

let o = object:
  method x = 1

(* attributes *)

let f = function: 
  [@blahblah] 
  | x -> x + 1

let x = lazy:
  [@blahblah]
  prerr_endline "hello";
  prerr_endline "world"

let o = object:
  [@blahblah]
  method x = 1

(* EOL tokens have indent 0 and must be ignored! *)
let f e =
  if e then:

    print_endline "true!";

    42

  else:

    print_endline "false!";

    -1 (* You cannot write ;; here *)
