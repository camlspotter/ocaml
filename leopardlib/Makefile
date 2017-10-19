OCAMLC=../ocamlc
OCAMLDEP=../tools/ocamldep

.PHONY: all
all: leopard.cmo test.exe

test.exe: leopard.cmo test.cmo 
	$(OCAMLC) -o $@ leopard.cmo test.cmo

.PHONY: clean
clean:
	rm -f *.cm*

.SUFFIXES: .cmo .cmi .ml .mli

.ml.cmo:
	$(OCAMLC) -c -o $@ $<

.mli.cmi:
	$(OCAMLC) -c -o $@ $<

.PHONY: depend
depend: .depend

.depend: *.ml *.mli
	$(OCAMLDEP) *.ml *.mli > $@

include .depend
