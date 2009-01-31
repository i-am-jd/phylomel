MAKE=make
# Uncomment the following line on WIN32
# MAKE=make WIN32=true

all: sdl

sdl:
	$(MAKE) -f makefile.inc MLFILE=phylog
	
clean:
	$(MAKE) -f makefile.inc MLFILE=phylog clean

test:
	$(MAKE) -f makefile.inc MLFILE=test