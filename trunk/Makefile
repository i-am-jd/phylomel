.PHONY: all
all: lib bin

.PHONY: lib
lib:
	@cd lib && $(MAKE)

.PHONY: bin
bin: lib
	@cd bin && $(MAKE)

.PHONY: install
install:
	@cd lib && $(MAKE) $@

.PHONY: uninstall
uninstall:
	@cd lib && $(MAKE) $@

.PHONY: clean
clean:
	@cd lib && $(MAKE) clean
	@cd bin && $(MAKE) clean
	@cd gltest && $(MAKE) clean

.PHONY: doc
doc: all
	@cd lib && $(MAKE) htdoc

.PHONY: gltest
gltest: lib
	@cd gltest && $(MAKE)
