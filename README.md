OCamleopard / 大麒麟 
==================================

OCamleopard / 大麒麟 (or simply "+leopard") is a collection of small modifications 
to OCaml. Currently it includes:

* Variant constructor as functions: `Some : 'a -> 'a option` (See README_curried_constr.md)
* Haskell style value-type declaration: `let f : 'a. 'a -> 'a and f x = x` (See README_haskellish_type.md)
* Python like indentation rule (See README_indent.md)
* Line comment: `/// comment` (See README_linecomment.md)
* Pattern guards `with p <- e ->` (See README_pattern_guard.md)
* Function as infix operator: ```24 ``div 8 = 3``` (See README_backquote_infix.md)
* SML style local name space by ``let:`` (See README_sml_let.md)
* η-expansion syntax sugar: `let id x = x;; let id2 = & id id;;`

Trivia of giraffes
-----------------------------------

Giraffe was known as "cameleopardalis" by ancient Greeks and Romans 
who considered it was a hybrid of a camel and a leopard 
which had a camel's hump and leopard's coat patterns.

Giraffe was imported to ancient China by Zhèng Hé(鄭和)'s fleet in 1419.
The emperor named it "qílín"(麒麟) since its look had some similarity with
the description of the mystical sacred animal with the same name, 
which was believed to appear only when the country was led 
by a wise and merciful ruler. It seems that the emperor had some problem 
in his self-esteem just as we have.

The word "qílín" is still used for giraffe in Japan as キリン(Kirin) 
and Korea as 기린(I do not read Korean so I do not know the correct pronunciation.). 
Of course none in Japan or Korea believes the myth today.

The meaning of chinese character "大" is big, and can be pronounsed 
as "O" in Japanese. Therefore OCamleopard can be translated to "大麒麟",
"big sacred animal".

駱駝(camels) were rare in China capitals but Camelus bactrianus 
in the central asia were known better than 麒麟(giraffes), 
therefore they did not win the sacred status like giraffes, unfortunately.
