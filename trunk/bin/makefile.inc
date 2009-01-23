# To use file, set MLFILE

OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = $(MLFILE).ml
PACKS = extlib
RESULT = $(MLFILE)

INCDIRS = ../lib
LIBS = phylomel

all: native-code

-include $(OCAMLMAKEFILE)
