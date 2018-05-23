module Add = struct
  let int = (+)
  let float = (+.)
end

let add = [%imp open_imp]

let () = let open %imp Add in assert (add 1 2 = 3)

