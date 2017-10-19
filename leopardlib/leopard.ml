external (+)   : 'a -> 'a -> 'a = "%OVERLOADED"
external (-)   : 'a -> 'a -> 'a = "%OVERLOADED"
external (/)   : 'a -> 'a -> 'a = "%OVERLOADED"
external ( * ) : 'a -> 'a -> 'a = "%OVERLOADED"

module Int = struct:
  let (+) = (+)
  let (-) = (-)
  let ( * ) = ( * )
  let (/) = (/)

module Float = struct:
  let (+) = (+.)
  let (-) = (-.)
  let ( * ) = ( *. )
  let (/) = (/.)

  
