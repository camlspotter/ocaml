module Add = struct
  let int = (+)
  let float = (+.)
end

let double ~_d x = [%imp Add] ~_d x x


  
