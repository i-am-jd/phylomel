# To use file, set MLFILE

MAKE=make

all: upgma2svg phylog phylogf mlva_eu

simpson:
	$(MAKE) -f makefile.inc MLFILE=simpson

upgma2svg:
	$(MAKE) -f makefile.inc MLFILE=upgma2svg

phylog:
	$(MAKE) -f makefile.inc MLFILE=phylog

phylogf:
	$(MAKE) -f makefile.inc MLFILE=phylogf

mlva_eu:
	$(MAKE) -f makefile.inc MLFILE=mlva_eu

clean:
	$(MAKE) -f makefile.inc MLFILE=upgma2svg clean	
	$(MAKE) -f makefile.inc MLFILE=simpson clean
	$(MAKE) -f makefile.inc MLFILE=phylog clean
	$(MAKE) -f makefile.inc MLFILE=phylogf clean
	$(MAKE) -f makefile.inc MLFILE=mlva_eu clean
