# Camleopard

> I will give an account of the so-called camelopard, because it was then for the first time introduced into Rome by Caesar and exhibited to all. This animal is in general a camel, except that it has sets of legs not of equal length. That is, its hind legs are shorter. Beginning from the rump its back grows gradually higher, appearing as if it would ascend indefinitely, until the most of its body reaching its loftiest point is supported on the front legs, while the neck stretches up to an unusual height. It has skin spotted like a leopard, and for this reason bears the name common to both animals.
>
> Dio, Roman History (XLIII.23.1-2) (http://www.gutenberg.org/cache/epub/11607/pg11607-images.html)

Camleopard is a modified OCaml compiler with several enhancements in its parsing, typing and complation:

* Syntax with Python style indentation rules
* ADT constructors as functions in both uncurried and curried form
* User definable SML style simple overloading

Even with these enhancements, it is designed to be compatible with OCaml as possible.  Camleopard can be used with OCaml together:

* Even with the indentation rules, the syntax is still upper-compatible: it can parse the original OCaml code.
* The existing OCaml PPX preprocessors can work with Camleopard, since its parsed AST is identical to the one of OCaml.
* Object files of OCaml and Camleopard can be used together, since Camleopard's middle and back end system is identical to those of OCaml.

To minimize the compilation bugs caused by its extensions, 
Camleopard has a two phased type checking: 
in the first typing phase, the input program is type-checked by Camleopard's extended type system.
Then, the typed AST is transformed to a vanilla OCaml code.
This output is type checked again in the second typing phase by the type checker without any Camleopard extensions to make sure that the output of Camleopard is at least OCaml type safe.
Camleopard is a standalone compiler, but it is also usable as a preprocessor to vanilla OCaml ASTs.  The output can be type-checked and compiled by the genuine OCaml compiler.
