let r = [%leopard {| {. x = 1 } |}]
let () = print_int [%leopard {| r..x |}] ; print_newline ()

[%%leopard {|

let x = 1
let y = 2

|}]

let r = [%leopard {|     let x = 1 in in x  (*fdsafsda *)
        |}]
