OCAMLC=../ocamlc
OCAMLOPT=../ocamlopt
OCAMLDEP=../tools/ocamldep

.PHONY: all
all: leopard.cmo test.exe

.PHONY: opt
opt: leopard.cmx 

test.exe: leopard.cmo test.cmo 
	$(OCAMLC) -o $@ leopard.cmo test.cmo

.PHONY: clean
clean:
	rm -f *.cm*

.SUFFIXES: .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) -c -o $@ $<

.mli.cmi:
	$(OCAMLC) -c -o $@ $<

.ml.cmx:
	$(OCAMLOPT) -c $<

.PHONY: depend
depend: .depend

.depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli > $@

include .depend

