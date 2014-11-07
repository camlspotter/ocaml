let () =
  let f : 'a. 'a -> 'a
  and f x = x
  in f ()

let () =
  let f : 'a. 'a -> 'a = [%val]
  and f x = x
  in f ()

let f : 'a. 'a -> 'a = [%val]
and f x = x

let () = f ()

let f : 'a. 'a -> 'a
and f x = x

let () = f ()


