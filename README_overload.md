SML style overloading
===============================

SML style overloading is very simple way to overload things. Much simpler than Haskell type classes, so you cannot derive overloading from overloaded values. You can get the idea from my past [article](http://camlspotter.blogspot.sg/2011/09/small-patch-for-bizarre-but-user.html). Let's try to overload `(+)` here too.

The design of the mod of this time is as follows. We need a seed of an overloaded value, with a polymorphic type, but without any actual definition. Fortunately, we have a way for this in OCaml: primitive declaration:

```ocaml
module Loaded = struct
  external (+) : 'a -> 'a -> 'a = "%OVERLOADED"
end
```

Here we declare `Loaded.(+)` to be a polymorphic function whose implementation is by a primitive named `%OVERLODED`. But we do not give any deinition of the primitive. The name `%OVERLOADED` is just a mark for our overloading. Very luckily, we can have such a fake polymorphic value in OCaml as far as it is never actually used.

In this `Loaded` module, we stack sub-modules which provide overloaded instances for this `(+)`:

```ocaml
module Loaded = struct
  external (+) : 'a -> 'a -> 'a = "%OVERLOADED"
  module Int = struct
    let (+) = Pervasives.(+)
  end
  module Float = struct
    let (+) = Pervasives.(+.)
  end
end
```

Here we have pluses for `int` and `float`. Now the preparation is done! Let's use `Loaded.(+)` as if it is overloaded by these two instances!:

```ocaml
open Loaded
let _ = 
  assert (1 + 2 = 3);
  assert (1.2 + 3.4 = 4.6) (* See it is not +. but + !!! *)
```
