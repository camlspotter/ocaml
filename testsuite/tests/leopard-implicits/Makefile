CMOS=lib.cmo

BASEDIR=../..
include $(BASEDIR)/makefiles/Makefile.okbad2
include $(BASEDIR)/makefiles/Makefile.common
ADD_COMPFLAGS=-no-retype -I ../../../leopardlib
# We have to link leopard.cmo explicltly, since -nostdlib is used.
ADD_LINKFLAGS=-I ../../../leopardlib leopard.cmo
