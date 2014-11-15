let x : 'a ref -> 'a = (.contents)
let x : 'a ref -> 'a = (!).contents
let x : < m : 'a; ..  > -> 'a = (#m)

