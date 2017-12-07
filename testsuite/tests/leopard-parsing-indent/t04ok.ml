let f x = 1 :@ hello

let g x = 2 :@@ hello

let h x = 3 :@ hello
  world

let _ =
  let i x =
    3 :@ hello
     world
  in
  i


