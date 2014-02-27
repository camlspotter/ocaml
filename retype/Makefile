#######################################################################
#                                                                     #
#                            OCamlSpotter                             #
#                                                                     #
#                             Jun FURUSE                              #
#                                                                     #
#   Copyright 2008-2012 Jun Furuse. All rights reserved.              #
#   This file is distributed under the terms of the GNU Library       #
#   General Public License, with the special exception on linking     #
#   described in file LICENSE.                                        #
#                                                                     #
#######################################################################


# Various commands and dir
##########################
CAMLRUN= ocamlrun
OCAMLC   = ocamlc -annot -bin-annot -w A-4-9-27-45 -warn-error A-4-9-10-27-32-33-34-39
OCAMLOPT = ocamlopt -annot -bin-annot -w A-4-9-27-45 -warn-error A-4-9-32-33-34-39
OCAMLDEP = ocamldep
OCAMLLEX = ocamllex
OCAMLYACC= ocamlyacc
OCAMLLIB = $(LIBDIR)
OCAMLBIN = $(BINDIR)

# Compilation
#############
OCAMLSRCDIR=..

INCLUDE_DIRS= parsing typing utils driver bytecomp

INCLUDES_DEP= $(addprefix -I $(OCAMLSRCDIR)/, $(INCLUDE_DIRS)) 

# Requires unix!
COMPFLAGS= $(INCLUDES_DEP)

MODULES= untypeast my_compile main

OBJS=		$(addsuffix .cmo, $(MODULES))

XOBJS=		$(addsuffix .cmx, $(MODULES))

retype: $(OBJS) $(OCAMLSRCDIR)/compilerlibs/ocamlcommon.cma $(OCAMLSRCDIR)/compilerlibs/ocamlbytecomp.cma
	ocamlc -o $@ -I $(OCAMLSRCDIR)/compilerlibs ocamlcommon.cma ocamlbytecomp.cma $(OBJS)

clean::
	rm -f retype

main.ml: $(OCAMLSRCDIR)/driver/main.ml
	sed -e 's/Compile\./My_compile./g' $< > $@

beforedepend:: main.ml

clean::
	rm -f main.ml

# generic rules :
#################

.SUFFIXES: .mll .mly .ml .mli .cmo .cmi .cmx

.ml.cmo:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.mli.cmi:
	$(OCAMLC) $(OCAMLPP) $(COMPFLAGS) -c $<

.ml.cmx:
	$(OCAMLOPT) $(OCAMLPP) $(COMPFLAGS) -c $<

.mll.ml:
	$(OCAMLLEX) $<

.mly.ml:
	$(OCAMLYACC) -v $<

.mly.mli:
	$(OCAMLYACC) -v $<

beforedepend::

depend: beforedepend
	ocamldep $(INCLUDES) *.mli *.ml > .depend

clean::
	rm -f *.cm* *.annot

include .depend

.PHONY: bootstrap

bootstrap: retype
	cp ../boot/ocamlc ../boot/ocamlc-retype-backup
	cp retype ../boot/ocamlc
	(cd ..; make core)
