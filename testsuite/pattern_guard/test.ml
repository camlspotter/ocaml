let _ =
  match 3, 2 with
  | (x, y) when x = y -> prerr_endline "0"
  | (x, y) with w <- x + y when w = 4 -> prerr_endline "1"
  | (x, y) with Some x <- None -> prerr_endline x
  | (x, y) with w <- x + y when w = 5 -> prerr_endline "3"
  | (x, y) when x = y + 1 when x = y + 1 -> prerr_endline "4"
  | _ -> prerr_endline "_"

let _ =
  match 3, 2 with
  | (x, y) when [%guard x = y] -> prerr_endline "0"
  | (x, y) when [%guard let w = x + y;; w = 4] -> prerr_endline "1"
  | (x, y) when [%guard let Some x = None]  -> prerr_endline x
  | (x, y) when [%guard let w = x + y;; w = 5] -> prerr_endline "3"
  | (x, y) when [%guard x = y + 1;; x = y + 1] -> prerr_endline "4"
  | _ -> prerr_endline "_"

let () = 
  let f = function
    | (x, y) when x = y -> prerr_endline "0"
    | (x, y) with w <- x + y when w = 4 -> prerr_endline "1"
    | (x, y) with Some x <- None -> prerr_endline x
    | (x, y) with w <- x + y when w = 5 -> prerr_endline "3"
    | (x, y) when x = y + 1 when x = y + 1 -> prerr_endline "4"
    | _ -> prerr_endline "_"
  in
  f (3,2)
