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

# Installation

## From Git repo with OPAM

```shell
$ opam pin add leopard git://github.com/camlspotter/ocaml#4.05.0+leopard
```

Once installed, you can check it working:

```shell
$ cd /tmp
$ echo "let x = Some" > x.ml
$ leopardc -i x.ml
val x : 'a -> 'a option
$ ocamlfind ocamlc -verbose -package leopard -syntax leopard -i x.ml
Effective set of preprocessor predicates: preprocessor,syntax,leopard
Effective set of compiler predicates: pkg_leopard,syntax,autolink,byte
+ ocamlc.opt -verbose -i -I <OPAMROOT>/4.05.0/lib/leopard -pp "leopardc '-I' '<OPAMROOT>/4.05.0/lib/leopard' '-as-pp' " x.ml
+ leopardc '-I' '<OPAMROOT>/4.05.0/lib/leopard' '-as-pp'  'x.ml' > /tmp/ocamlppd1cbe5
val x : 'a -> 'a option
```

## From a local source

Or, if you have a copy of the source code:

```shell
$ opam pin add --kind path leopard .
```

## As an indenpendent compiler swith (TBD)

You can also install Camleopard as an indepdendent standalone compler
as a new switch. (TBD)

# How to use

There are several ways to use Camleopard.  Each way has pros and cos.

## As modified standalone compilers, coexisting with the original compilers

Modified compilers and tools `leopardc`, `leopardopt`, `leoparddep`,
etc. are provided as replacements of the original compilers and tools.

Pros:

* No dependency trick required

Cons:

* Need to set an environment varialbe for `ocamlfind`, which may be tricky for some build systems. (See details below)
* If you are paranoiac, you are not happy since there is no type guarantee by the genuine OCaml compiler.

### Work with Findlib (i.e. `ocamlfind`)

To use the modified compilers and tools with `ocamlfind` command,
you have to set an environment variable `OCAMLFIND_COMMANDS`
(only) where you need Camleopard:

```shell
OCAMLFIND_COMMANDS='ocamlc=leopardc ocamlopt=leopardopt ocamldep=leoparddep
```

### Work with Findlib (i.e. `ocamlfind`) (Plan B)

You MAY ALSO add the following lines to `findlib.conf` by hand:

```
ocamlc(leopard)="leopardc"
ocamlopt(leopard)="leopardopt"
ocamldep(leopard)="leoparddep"
ocamldoc(leopard)="leoparddoc"
```

then give `-toolchain leopard` option to `ocamlfind` commands. For example,

```shell
$ ocamlfind -toolchain leopard ocamlc /tmp/x.ml
```

In OPAM environment, `findlib.conf` should be found at the directry 
which `opam config var lib` prints out.

## As a preprocessor

Camleopard can be used as a preprocessor which transforms Camleopard
code to vanilla OCaml, which can be compiled with the original compiler.
By adding an option `-pp "leopardc -as-pp"` to the original compiler commands,
Camleopard compilers preprocess the input Camleopard programs to vanilla OCaml,
then the output is passed to the genuine OCaml compilers.

Note that the same preprocessor option `-pp "leopardc -as-pp" CANNOT be used
for dependency analysis (`ocamldep`), since `leopardc -as-pp` tries to type
check the target module.  This usually fails because the modules on which 
the targets depend are not compiled yet at the dependncy analysis.
Instead, `ocamldep` must have an option `-pp "leopardc -as-pp -no-trans"`:

```
ocamldep -pp "leopardc -as-pp -notrans" /tmp/x.ml
```

or you may also use `leoparddep /tmp/xml`.

Pros

* Transformed code is type double-checked and compiled by the genuine OCaml compiler.

Cons

* Dependency analysis (`ocamldep`) requires a special treatment.
* Needs to add extra dependencies over preprocessing in some build systems  (See details below)

### Work with Findlib (i.e. `ocamlfind`)

With `ocamlfind`, you can use Camleopard as a preprocessor as follows:

```
$ ocamlfind ocamlc -package leopard -syntax leopard /tmp/x.ml
$ ocamlfind ocamlopt -package leopard -syntax leopard /tmp/x.ml
$ ocamlfind ocamldep -package leopard -syntax leoparddep /tmp/x.ml
```

Note that you have to provide a different syntax option `-syntax leoparddep` 
for `ocamldep`.

### Preprocessing now depends on types of other modules

Note that when Camleopard is used as a preprocessor 
with its type system extensions,
its preprocessing is dependent not only on the source code itself 
but also on its type environment: the types of the modules it depends on.
You may need to add this extra preprocessing dependency over other modules
to your build system, if such a build system assumes preprocessing is 
purely dependent on the source code (i.e. `jbuilder`).

# Additional command line options

Camleopard compilers and tools have the following additional command line
options to the original OCaml compilers and tools.

## `-as-pp`

Option `-as-pp` makes compilers as preprocessors.  Instead of generating
object files it prints out the result of their program transformations in
vanilla OCaml binary untyped AST, so that they can be type double-checked 
and compiled by the genuine OCaml compilers.

## `-as-pp-text`

Same as `-as-pp` option, but `-as-pp-text` prints out the result of program
transformations in human readable vanilla OCaml code.  It is good to see
how Camleopard code transformation works.

Note that `-as-pp-text` is only for debugging and information purposes:
the output of `-as-pp-text` is not always a valid OCaml program.

## `-no-retype`

Option `-no-retype` disables type double-checking of Camleopard standalone
compilers.

Compiler modification is a subtle work and there is always a risk of mistake,
and such a mistake may affect the behaviour of the compiled programs
in unexpected ways.  Camleopard tries to minimize the risk 
by type double-checking: once its program transformation is done, 
the final typed AST is untyped back to a untyped AST, then type-checked
again by its type system without any Camleopard extensions, which should be
identical to the genuine OCaml compiler's.  Thus Camleopard at least assures
that its compiled programs are from well-typed OCaml codes.

Option `-no-retype` omits this type double-check for compilation speed.

## `-no-trans`

The main use of `-no-trans` is to feed Camleopard programs to 
`ocamldep` dependency analyzer.

Option `-no-trans` disables Camleopard program transformation.
This option makes compilers as preprocessors just like `-as-pp`, but
it only desugars Camleopard syntactic sugar and does not perform
its program transformation.
The output, if successful, is a valid vanilla OCaml binary untyped AST.
However the output is often not compilable by the genuine OCaml compilers
since it may contain Camleopard extension points.

The output of `-no-trans` can be compiled by Camleopard compilers.

