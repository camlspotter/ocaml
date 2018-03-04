OCAMLRUN=../boot/ocamlrun 
OPTIONS=-nostdlib -I ../stdlib
OCAMLC= $(OCAMLRUN) ../ocamlc $(OPTIONS)
OCAMLOPT= $(OCAMLRUN) ../ocamlopt $(OPTIONS)
OCAMLDEP= $(OCAMLRUN) ../tools/ocamldep

.PHONY: allopt
leopardlib: opt

.PHONY: all
all: leopard.cmo

.PHONY: allopt
leopardlib: opt

.PHONY: opt
opt: leopard.cmx 

.PHONY: clean
clean:
	rm -f *.cm* *.exe

.PHONY: partialclean
partialclean: clean

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

