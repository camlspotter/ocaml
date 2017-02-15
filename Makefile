#**************************************************************************
#*                                                                        *
#*                                 OCaml                                  *
#*                                                                        *
#*            Xavier Leroy, projet Cristal, INRIA Rocquencourt            *
#*                                                                        *
#*   Copyright 1999 Institut National de Recherche en Informatique et     *
#*     en Automatique.                                                    *
#*                                                                        *
#*   All rights reserved.  This file is distributed under the terms of    *
#*   the GNU Lesser General Public License version 2.1, with the          *
#*   special exception on linking described in the file LICENSE.          *
#*                                                                        *
#**************************************************************************

# The main Makefile

MAKEREC=$(MAKE)
include Makefile.shared

# Compile also native code compiler and libraries, fast
world.opt:
	$(MAKE) coldstart
	$(MAKE) opt.opt

# Hard bootstrap how-to:
# (only necessary in some cases, for example if you remove some primitive)
#
# make coreboot     [old system -- you were in a stable state]
# <change the source>
# make clean runtime coreall
# <debug your changes>
# make clean runtime coreall
# make coreboot [new system -- now in a stable state]

natruntop:
	$(MAKE) runtime
	$(MAKE) coreall
	$(MAKE) opt.opt
	$(MAKE) ocamlnat
	@rlwrap --help 2>/dev/null && rlwrap $(NATRUNTOP) || $(NATRUNTOP)

.PHONY: world.opt

include .depend
