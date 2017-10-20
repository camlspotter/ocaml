# OCamleopard

> I will give an account of the so-called *camelopard*, because it was then for the first time introduced into Rome by Caesar and exhibited to all. This animal is in general a *camel*, except that it has sets of legs not of equal length. That is, its hind legs are shorter. Beginning from the rump its back grows gradually higher, appearing as if it would ascend indefinitely, until the most of its body reaching its loftiest point is supported on the front legs, while the neck stretches up to an unusual height. It has skin spotted like a *leopard*, and for this reason bears the name common to both animals.
>
> Dio, Roman History (XLIII.23.1-2) (http://www.gutenberg.org/cache/epub/11607/pg11607-images.html)

OCamleopard is a modified OCaml compiler with several enhancements in its parsing, typing and complation:

* Syntax with Python style indentation rules
* Variant constructors as functions in both uncurried and curried form
* User definable SML style simple overloading

Even with these enhancements, it is designed to be compatible with OCaml as possible.  OCamleopard can be used with OCaml together:

* Even with the indentation rules, the syntax is still upper-compatible: it can parse the original OCaml code.
* The existing OCaml PPX preprocessors can work with OCamleopard, since its parsed AST is identical to the one of OCaml.
* Object files of OCaml and OCamleopard can be used together, since OCamleopard's middle and back end system is identical to those of OCaml.

To minimize the compilation bugs caused by its extensions, 
OCamleopard has a two phased type checking: 
in the first typing phase, the input program is type-checked by OCamleopard's extended type system.
Then, the typed AST is transformed to a vanilla OCaml code.
This output is type checked again in the second typing phase by the type checker without any OCamleopard extensions to make sure that the output of OCamleopard is at least OCaml type safe.
OCamleopard is a standalone compiler, but it is also usable as a preprocessor to vanilla OCaml ASTs.  The output can be type-checked and compiled by the genuine OCaml compiler.

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

You can also install OCamleopard as an indepdendent standalone compler
as a new switch. (TBD)

# How to use

There are several ways to use OCamleopard.  Each way has pros and cos.

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
(only) where you need OCamleopard:

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

OCamleopard can be used as a preprocessor which transforms OCamleopard
code to vanilla OCaml, which can be compiled with the original compiler.
By adding an option `-pp "leopardc -as-pp"` to the original compiler commands,
OCamleopard compilers preprocess the input OCamleopard programs to vanilla OCaml,
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

With `ocamlfind`, you can use OCamleopard as a preprocessor as follows:

```
$ ocamlfind ocamlc -package leopard -syntax leopard /tmp/x.ml
$ ocamlfind ocamlopt -package leopard -syntax leopard /tmp/x.ml
$ ocamlfind ocamldep -package leopard -syntax leoparddep /tmp/x.ml
```

Note that you have to provide a different syntax option `-syntax leoparddep` 
for `ocamldep`.

### Preprocessing now depends on types of other modules

Note that when OCamleopard is used as a preprocessor 
with its type system extensions,
its preprocessing is dependent not only on the source code itself 
but also on its type environment: the types of the modules it depends on.
You may need to add this extra preprocessing dependency over other modules
to your build system, if such a build system assumes preprocessing is 
purely dependent on the source code (i.e. `jbuilder`).

# Additional command line options

OCamleopard compilers and tools have the following additional command line
options to the original OCaml compilers and tools.

## `-as-pp`

Option `-as-pp` makes compilers as preprocessors.  Instead of generating
object files it prints out the result of their program transformations in
vanilla OCaml binary untyped AST, so that they can be type double-checked 
and compiled by the genuine OCaml compilers.

## `-as-pp-text`

Same as `-as-pp` option, but `-as-pp-text` prints out the result of program
transformations in human readable vanilla OCaml code.  It is good to see
how OCamleopard code transformation works.

Note that `-as-pp-text` is only for debugging and information purposes:
the output of `-as-pp-text` is not always a valid OCaml program.

## `-no-retype`

Option `-no-retype` disables type double-checking of OCamleopard standalone
compilers.

Compiler modification is a subtle work and there is always a risk of mistake,
and such a mistake may affect the behaviour of the compiled programs
in unexpected ways.  OCamleopard tries to minimize the risk 
by type double-checking: once its program transformation is done, 
the final typed AST is untyped back to a untyped AST, then type-checked
again by its type system without any OCamleopard extensions, which should be
identical to the genuine OCaml compiler's.  Thus OCamleopard at least assures
that its compiled programs are from well-typed OCaml codes.

Option `-no-retype` omits this type double-check for compilation speed.

## `-no-trans`

The main use of `-no-trans` is to feed OCamleopard programs to 
`ocamldep` dependency analyzer.

Option `-no-trans` disables OCamleopard program transformation.
This option makes compilers as preprocessors just like `-as-pp`, but
it only desugars OCamleopard syntactic sugar and does not perform
its program transformation.
The output, if successful, is a valid vanilla OCaml binary untyped AST.
However the output is often not compilable by the genuine OCaml compilers
since it may contain OCamleopard extension points.

The output of `-no-trans` can be compiled by OCamleopard compilers.

# Syntax with Python style indentation rules

OCamleopard introduces special keywords `with:`, `then:`, `else:`, `do:`, `function:` etc.
to start indentation aware blocks.  These blocks have no closing keywords but closed by
code indentations.

Note that even with these Python style indentation keywords, OCamleopard's syntax is
100% backward compatible with the original OCaml.  The normal code block starting symbol
`begin`, `do`, `struct`, etc. must be closed explicitly by their counterparts like
`end` and `done`.

## Example

Special keywords end with `:` introduce implicit blocks based on 
the code indentation.

The following program using two special keywords `then:` and `else:`:

```ocaml
let f e =
  if e then:
    print_endline "true!";
    42
  else:
    print_endline "false!";
    -1
in
...
```

is equivalent to the original OCaml code below:

```ocaml
let f e = 
  if e then begin              (* <- begin inserted *)
    print_endline "true!";
    42
  end else begin               (* <- end and begin inserted *)
    print_endline "false!";
    -1
  end                          (* <- end inserted *)  
in
...
```

This Python style `xxx:` keywords are introduced all the paired keywords
like `do` and `done`, `object` and `end`, etc. and all the open keywords
without the corresponding closings such as `match .. with`, `try .. with`,
`funciton`.

`do:` does not require the paired `done`:

```ocaml
for i = 1 to 100 do:
  print_int i;                   (* you still need to type ; *)
print_endline "printed 100"      (* lowering indent implicitly closes do: *)
```

is equilvalent with

```ocaml
for i = 1 to 100 do
  print_int i;
done;
print_endline "printed 100"
```

`with:` introduces implicit `begin` and `end`, useful in nested matches:

```ocaml
match xs with
| [] ->
  match y with:    (* introduces implicit begin *)
  | A -> 2
  | B -> 3
| x::xs ->         (* lowering indent implicitly closes the begin *)
  match y with:    (* introduces implicit begin *)
  | A -> 4
  | B -> 5         (* EOF implicitly closes the begin *)
```
                       
is equlivalent with 

```ocaml
match xs with
| [] ->
  begin match y with
  | A -> 2
  | B -> 3
  end
| x::xs ->
  begin match y with
  | A -> 4
  | B -> 5
  end
```
You can see more samples at `testsuite/tests/parsing-indent/t01ok.ml` of the OCamleopard 
source code.

## Special keywords

Auto-insertion of `begin` .. `end` for the following keywords:

* `with:`, `then:`, `else:`, `function:` and `lazy:`

Auto-insertion of the corresponding ending keywords for the following:

* `do:` (`done` is not required)
* `sig:`, `struct:`, `object:` (`end` is not required)

## When the implicit closings are inserted?

The implicit closing of a special keyword `xxx:` happens when 
the indentation level goes to *less than* or *equal to* 
the indentation level where the special keyword is introduced:

```ocaml
for i = 0 to 100 do:         (* indent level 0 *)
  print_int i;               (* level 2 *)
print_endline "printed 100!" (* back to 0, we must close the do: *)
```

is equivalent to

```ocaml
for i = 0 to 100 do
  print_int i;
done;
print_endline "printed 100!"
```

Note that this is not the horizontal level of the special keyword:

```ocaml
match x with
  p -> function:   (* indent level 2 at p. Not 7 at function: *)
    | A -> 1       (* level 4. No closing happens *) 
    | B -> 2
| q -> ...         (* Level 0. *)
```

is equvalient with

```ocaml
match x with
  p -> begin function
    | A -> 1
    | B -> 2
    end
| q -> ...
```

### Indentation of the line starts with `|`

The indentation level of the line which starts with the vertical bar `|` 
for the pattern matches is treated a bit differently, in order to support
the common indentation convention of or-patterns: they are often leveled
at the same with `match`, `funciton` and `try`. 

At the lines starts with `|`, the auto closing only happens when their
indentation levels are *strictly less than* those of the lines with 
the corresponding special keywords:

```ocaml
let rec f x = 
  match x mod 3, x mod 5 with:    (* level 2. Introduces an implicit begin. *)
  | 0, 0 -> print_string "fizbuz" (* level 2. This does not close the implicit begin *)
  | 0, _ -> print_string "fiz"    (* level 2. *)
  | _, 0 -> print_string "buz"    (* level 2. *)
  | _ -> print_int x;             (* level 2. *)
  f (x+1)                         (* level 2. Start not with |. with: must be closed *)
```

is equivalent to

```ocaml
let rec f x = 
  begin match x mod 3, x mod 5 with
  | 0, 0 -> print_string "fizbuz"
  | 0, _ -> print_string "fiz"
  | _, 0 -> print_string "buz"
  | _ -> print_int x;
  end;
  f (x+1)
```

### Handling of `;`

If `;` symbol for sequential execution `e1; e2` appears at the end of a line
and implicit closing happens just after this line, then the `;` is treated
as if it is after the closing:

```ocaml
for i = 0 to 100 do:
  print_int i;                   (* ; appears at the end of do: indentation block *)
print_endline "printed 100!"
```
            
is equilvalent to

```ocaml
for i = 0 to 100 do
  print_int i;
done;                            (* ; is used to sequence the next line *)
print_endline "printed 100!"
```

## Note

### `xxx:` must be at the end of lines

After the special keywords, you must immediately change the line:

```ocaml
match e1 with: p -> e2
```

is rejected as a syntax error. You can still write comments:

```ocaml
match e1 with: (* special keyword! *)
| p -> e2
```

is ok.

### Attributes

Special `xxx:` keywords whose original versions can take attributes
are also able to take attributes, after `:` signs changing the line:

```ocaml
function:
[@blahblah]
| p -> e
```

The line changing is mandatory. `function: [@blahblah]` may look better 
but it is not possible for the current implementation approach 
as a simple lexer level converter.

## Behind the scene

This hack is done by lexer level preprocessing.  The OCamleopard lexer preprocessor
sits between the original OCaml's lexer and parser.  It receives a lexer stream
which contains special keywords `xxx:` and convert them to their normal versions
and insert the closing keywords when necessary.

To make the hack around implicit `begin` introduction as simple as possible,
some syntaxes are (secretly) extended. For example, the following is valid with this patch:

```ocaml
match x mod 3, x mod 5 with begin (* begin after with *)
| 0, 0 -> print_string "fizbuz"
| 0, _ -> print_string "fiz"
| _, 0 -> print_string "buz"
| _ -> print_int x;
end
```

This variant is actually used for the desugared version of `match ... with:`.
Technically, for lexer based filter, it is hard to insert `begin` in front 
of `match` when it sees `with:` in its token stream.

# Variant constructors as functions in both uncurried and curried form

In OCamleopard, variant constructors can be used without applying its argument
just like partially applied functions. Suppose we have:

```ocaml
type t = Foo of int * float
```

Then

```ocaml
Foo
```

is equal to `fun (x,y) -> Foo (x,y)`. And,

```
!Foo
```

is equal to `fun x y -> Foo (x,y)`.

## Polymorphic variants as functions

```ocaml
!`Foo
```

is equivalent to

```ocaml
fun x -> `Foo x
```

## Samples

You can try examples at `testsuite/tests/typing-curried-constr/test.ml` in OCamleopard source code.

# User definable SML style simple overloading

SML style overloading is very simple way to overload things. Much simpler than Haskell type classes, so you cannot derive overloading from overloaded values. You can get the idea from my past [article](http://camlspotter.blogspot.sg/2011/09/small-patch-for-bizarre-but-user.html). Let's try to overload `(+)` here too.

To start with, we need a seed of an overloaded value, with a polymorphic type, 
but without any actual definition:

```ocaml
module Loaded = struct
  external (+) : 'a -> 'a -> 'a = "%OVERLOADED"
end
```

Here we declare `Loaded.(+)` to be a polymorphic function whose implementation is by a primitive named `%OVERLODED`. 
The name `%OVERLOADED` is just a mark for our overloading 
and we do not give any deinition of the primitive. 
Very luckily, we can have such a fake polymorphic value declaration in OCaml as far as it is never actually used.

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

Here we have `plus`es for `int` and `float`. 
Now the preparation is done! Let's use `Loaded.(+)` as if it is overloaded by these two instances!:

```ocaml
open Loaded
let _ = 
  assert (1 + 2 = 3);
  assert (1.2 + 3.4 = 4.6) (* See it is not +. but + !!! *)
```

The example of overloaded functions can be found at `testsuite/tests/overload/t01ok.ml` in OCamleopard source code.

# Trivia of giraffes

Giraffe was known as "cameleopardalis" by ancient Greeks and Romans who considered it was a hybrid of a camel and a leopard which had a camel's hump and leopard's coat patterns.

Giraffe was imported to ancient China by Zhèng Hé(鄭和)'s fleet in 1419. The emperor named it "qílín"(麒麟) since its look had some similarity with the description of the mystical sacred animal with the same name, which was believed to appear only when the country was led by a wise and merciful ruler. It seems that the emperor had some problem in his self-esteem just as we have.

The word "qílín" is still used for giraffe in Japan as キリン(Kirin) and Korea as 기린(I do not read Korean so I do not know the correct pronunciation.).  Of course none in Japan or Korea believes the myth today.  But you can still see the picture of the mystic animal in the logo of Kirin beer.

The meaning of chinese character "大" is big, and can be pronounsed as "O" in Japanese. Therefore OCamleopard can be translated to "大麒麟", "big sacred animal".

駱駝(camels) were rare in China capitals but Camelus bactrianus in the central asia were known better than 麒麟(giraffes), therefore they did not win the sacred status like giraffes, unfortunately.
