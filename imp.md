# Implicits

## Implicit values

Leopard provides a way to auto-generate a value of a given type `ty` 
using a given _candidate specification_ `spec`.
A candidate specification `spec` defines how to choose the candidate values to auto-generate
the value of the type `ty`.

The auto-generation of a value of type `ty` using candidate specification `spec` may fail
if it is impossible to build a value or there is any ambiguity.

### Implicit type

Leopard has a special type `(ty, spec) Leopard.Implicits.t`.
It is actually a phantom type equal to `ty` but its equality is hidden by the interface:

```
module Implicits : sig
  type ('ty, 'spec) t
  val get : ('ty, 'spec) t -> 'ty
  val embed : 'ty -> ('ty, 'spec) t
  ...
end = struct
  type ('ty, 'spec) t = 'ty
  external get : ('ty, 'spec) t -> 'ty = "%identity"
  external embed : 'ty -> ('ty, 'spec) t = "%identity"
  ...
end
```

Function `Leopard.Implicits.get` and `Leopard.Implicits.embed` provide the embeding.

## Implicit arguements

### Implicit arguments as special cases of optional arguments

Implicit arguments in Leopard are optional arguments whose labels start with character `_`.

For example,

```
val add ?_d:('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t -> 'a -> 'a -> 'a
```

The function `add` has an implicit argument labeled with `_d`.

### Implicit argument omission

If an implicit argument is omitted in an application, Leopard tries to build the omitted 
argument value based on the type of the implicit argument.

Suppose we have the same function `add` above:

```
val add ?_d:('a -> 'a -> 'a, [%imp Add]) Leopard.Implicits.t -> 'a -> 'a -> 'a
```

and used as follows:

```
let () = assert (add 1 2 = 3)
```

Here the implicit argument is omitted in the application.  Leopard auto-generates a value
of `(int -> int -> int, [%imp Add]) Leopard.Implicits.t` to be applied to the function.
Therefore, the above expression is equilvalent with:

```
let () = assert (add ~_d:<generated value> 1 2 = 3)
```

If Leopard fails to auto-generate the value of the type of the implicit argument,
the program is rejected at the typing phase.

### Explicitly giving `None` to implicit arguments

Giving `None` or expresions evaluate to `None` to an implicit arguments
causes a run-time error.

```
add ?_d:None 1 2   ===>  Error
```

Currently there is no type dicipline to prevent this.

