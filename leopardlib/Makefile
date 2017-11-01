OCAMLRUN=../boot/ocamlrun 
OPTIONS=-nostdlib -I ../stdlib
OCAMLC= $(OCAMLRUN) ../ocamlc $(OPTIONS)
OCAMLOPT= $(OCAMLRUN) ../ocamlopt $(OPTIONS)
OCAMLDEP= $(OCAMLRUN) ../tools/ocamldep

.PHONY: all
all: leopard.cmo test.exe test2.exe

.PHONY: opt
opt: leopard.cmx 

test.exe: leopard.cmo test.cmo 
	$(OCAMLC) -o $@ leopard.cmo test.cmo

test2.exe: leopard.cmo test2.cmo 
	$(OCAMLC) -o $@ leopard.cmo test2.cmo

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

