OCAMLMAKEFILE = ../OCamlMakefile

SOURCES = \
	distMat.mli \
	distMat.ml \
	genotype.mli \
	genotype.ml \
	genoMat.mli \
	genoMat.ml \
	dynMat.mli \
	dynMat.ml \
	dendogram.mli \
	dendogram.ml \
	vec2.ml \
	svg.ml \
	tree.mli \
	tree.ml \
	phylogram.ml \
	clustering.mli \
	clustering.ml \
	diversity.mli \
	diversity.ml \
	barnesHut.mli \
	barnesHut.ml \
	forceDirectedLayout.ml \

LIB_PACK_NAME = phylomel
OCAMLFLAGS = -for-pack Phylomel -dtypes -unsafe
PACKS = extlib
RESULT = phylomel

all: byte-code-library native-code-library

LIBINSTALL_FILES = \
	phylomel.cmi phylomel.cma phylomel.cmxa phylomel.a

install: libinstall
uninstall: libuninstall

clean :: clean-doc

DOC_DIR = ../doc

-include $(OCAMLMAKEFILE)