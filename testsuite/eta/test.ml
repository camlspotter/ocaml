let id x = x
let id2 = id id
let id3 = & id id
let id4 : 'a -> 'a = & id id
let id5 : 'a. 'a -> 'a = & id id

class c = object
  method id : 'a. 'a -> 'a = & id id
end
