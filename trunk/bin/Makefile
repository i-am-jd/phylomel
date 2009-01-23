# To use file, set MLFILE

MAKE=make

all: simpson upgma2svg phylog

simpson:
	$(MAKE) -f makefile.inc MLFILE=simpson

upgma2svg:
	$(MAKE) -f makefile.inc MLFILE=upgma2svg

phylog:
	$(MAKE) -f makefile.inc MLFILE=phylog

clean:
	$(MAKE) -f makefile.inc MLFILE=upgma2svg clean	
	$(MAKE) -f makefile.inc MLFILE=simpson clean
	$(MAKE) -f makefile.inc MLFILE=phylog clean
