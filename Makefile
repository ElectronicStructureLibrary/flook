
# Set the default VPATH
VPATH ?= $(shell pwd)

ARCH_MAKE_DEFAULT=$(VPATH)/arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

.PHONY: default
default: lib

.PHONY: all
all: lib test
	@echo Done with everything

.PHONY: lib
lib:
	(cd aotus ; ln -fs $(ARCH_MAKE) arch.make)
	$(MAKE) -C aotus lib
	$(MAKE) -C src lib

.PHONY: doc
doc:
	doxygen docs/Doxyfile

.PHONY: test check
check: test
test: lib
	$(MAKE) -C test "ARCH_MAKE=$(ARCH_MAKE)" all

.PHONY: clean
clean:
	(cd aotus ; rm -f arch.make)
	-rm -rf docs/html docs/latex
	-$(MAKE) -C aotus "ARCH_MAKE=$(ARCH_MAKE)" clean
	-$(MAKE) -C src "ARCH_MAKE=$(ARCH_MAKE)" clean
	-$(MAKE) -C test "ARCH_MAKE=$(ARCH_MAKE)" clean
