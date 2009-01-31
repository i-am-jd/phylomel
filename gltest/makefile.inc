LIBDIR=lib
BINDIR=.
SRCDIR=.

CLIBS=SDL

PACKS = extlib

INCDIRS = ../lib
LIBS = unix bigarray phylomel

OCAMLFLAGS="-unsafe"

SOURCE=utils.ml
SDL_SOURCE=$(LIBDIR)/sdl_stub.c $(LIBDIR)/sdl.mli $(LIBDIR)/sdl.ml
GL_SOURCE=$(LIBDIR)/glcaml_stub.c $(LIBDIR)/glcaml.mli $(LIBDIR)/glcaml.ml
ifdef NOSDL
	WIN_SOURCE=$(LIBDIR)/win_stub.c $(LIBDIR)/win.mli $(LIBDIR)/win.ml
	SDL_SOURCE=
	CLIBS=GL
	ifdef WIN32
	CLIBS=opengl32 gdi32
	endif
	LIBS=graphics unix bigarray
endif	
SRC=$(SRCDIR)/$(MLFILE).ml
CC=gcc
SOURCES=$(WIN_SOURCE) $(SDL_SOURCE) $(GL_SOURCE) $(SOURCE) $(SRC)
RESULT=$(BINDIR)/$(MLFILE)
-include OCamlMakefile
