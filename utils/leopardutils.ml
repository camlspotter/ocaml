module Open : sig
  (** Same as [(@@)], but slightly weaker *)
  external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"

  val flip : ('a -> 'b -> 'c) -> 'b -> 'a -> 'c
  val flip2 : ('a -> 'b -> 'c -> 'd) -> 'b -> 'c -> 'a -> 'd
end = struct
  (* (@@) is too strong *)
  external ( & ) : ('a -> 'b) -> 'a -> 'b = "%apply"
  let flip f x y = f y x
  let flip2 f x y z = f z x y
end
  
module XFormat = struct
  open Format

  type t = formatter

  type 'a fmt = t -> 'a -> unit

  let sprintf fmt =
    let buf = Buffer.create 100 in
    let ppf = formatter_of_buffer buf in
    kfprintf (fun ppf -> pp_print_flush ppf (); Buffer.contents buf) ppf fmt
  
  let ksprintf f fmt =
    let buf = Buffer.create 100 in
    let ppf = formatter_of_buffer buf in
    kfprintf (fun ppf -> pp_print_flush ppf (); f (Buffer.contents buf)) ppf fmt
  
  let wrapf left right ppf fmt =
    left ppf;
    kfprintf right ppf fmt
      
  let warnf fmt =
    wrapf
      (fun ppf -> fprintf ppf "@[<2>Warning: ")
      (fun ppf -> fprintf ppf "@]@.")
      err_formatter fmt

  let (!!%) = Format.eprintf

  module Open = struct
    let (!!%) = (!!%)

    let warnf = warnf
  end
end

module Format = struct
  include Format
  include XFormat
end
  
module Option = struct
  let map f = function
    | None -> None
    | Some v -> Some (f v)

  let to_list = function
    | Some x -> [x]
    | None -> []

  module Monad = struct
    let return x = Some x
    let some = return
    let (>>=) m f = match m with
      | None -> None
      | Some x -> f x
  end

  open Format
  let format f ppf = function
    | None -> pp_print_string ppf "None"
    | Some v -> fprintf ppf "@[<2>Some@ (@[%a@])@]" f v

  module Open = struct
    exception Is_None
    let from_Some = function
      | Some x -> x
      | None -> raise Is_None
  end
end

module XList = struct
  open List

  let rec filter_map f = function
    | [] -> []
    | x :: xs -> match f x with
      | None -> filter_map f xs
      | Some y -> y :: filter_map f xs

  let concat_map f xs = concat (map f xs)

  let assoc_opt x xs = try Some (assoc x xs) with _ -> None

  let partition_map f xs =
    let rec part left right = function
      | [] -> rev left, rev right
      | x::xs ->
          match f x with
          | `Left v -> part (v::left) right xs
          | `Right v -> part left (v::right) xs
    in
    part [] [] xs

  let from_to f t =
    (* CR jfuruse: we should build from 'to' *)
    let rec from_to st f t =
      if f > t then rev st
      else from_to (f::st) (f+1) t
    in
    from_to [] f t

  (* Haskell's splitAt. Borrowed from Spotlib. Tested. *)
  let split_at n xs =
    let rec split_at_ n st xs =
      if n <= 0 then st, xs
      else match xs with
      | [] -> st, []
      | x::xs -> split_at_ (n-1) (x::st) xs
    in
    let r, dropped = split_at_ n [] xs in
    rev r, dropped

  open Format   
  let rec format (sep : (unit, formatter, unit) format)  f ppf = function
    | [] -> ()
    | [x] -> f ppf x
    | x::xs -> 
        fprintf ppf "@[%a@]%t%a" 
  	f x
  	(fun ppf -> fprintf ppf sep)
  	(format sep f) xs
end 

module List = struct
  include List
  include XList
end

module XString = struct
  open String

  let is_prefix p s = try sub s 0 (length p) = p with _ -> false

  let drop len str = String.sub str len (String.length str - len)

  let is_prefix' ?(from=0) sub str =
    let sublen = String.length sub in
    try 
      if String.sub str from sublen = sub then Some (drop (from + sublen) str)
      else None
    with _ -> None
end 

module String = struct
  include String
  include XString
end

module XHashtbl = struct
  let to_list tbl = Hashtbl.fold (fun k v st -> (k,v) :: st) tbl []
end

module Hashtbl = struct
  include Hashtbl
  include XHashtbl
end

module XFilename = struct
  let split_extension s =
    let open String in
    try
      let pos = rindex s '.' in
      sub s 0 pos, sub s pos (length s - pos)
    with
    | _ -> s, "" 
end

module Filename = struct
  include Filename
  include XFilename
end

module Result = struct
  type ('a, 'err) t = ('a, 'err) result

  let at_Error f = function
    | Ok v -> v
    | Error e -> f e

  module Open = struct
    let at_Error = at_Error
  end
  
  module Monad = struct
    let (>>=) x f = match x with Error e -> Error e | Ok v -> f v
  end
end

module Exn = struct
  let protect f = try Ok (f ()) with e -> Error e

  let unprotect = function
    | Ok v -> v
    | Error e -> raise e

  let exit_then d f = try f () with Exit -> d

  module Open = struct
    let protect = protect
    let unprotect = unprotect
    let exit_then = exit_then
  end
end

module XSys = struct
  let env_exist s = try ignore (Sys.getenv s); true with _ -> false
end

module Sys = struct
  include Sys
  include XSys
end

include Open
include Format.Open
include Option.Open
include Result.Open
include Exn.Open
