
# Set the default VPATH
VPATH ?= $(shell pwd)

ARCH_MAKE_DEFAULT=$(VPATH)/arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

LIBTOOL ?= libtool
AR ?= ar
RANLIB ?= ranlib

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

# We assume that all the libraries
# do not have name clashes (which for these small libraries
# is not a problem)
# This simple routine extracts and comibes the
# object files into one unified library.
ifdef LUA_DIR
LOCAL__LUA_LIB ?= $(LUA_DIR)/lib/liblua.a
else
LOCAL__LUA_LIB ?= ../aotus/external/lua-5.3.0/src/liblua.a
endif
.PHONY: liball
liball: lib
	(mkdir -p .tmp_link ; cd .tmp_link ; \
	$(AR) x ../src/libflook.a ; \
	$(AR) x ../aotus/source/libaotus.a ; \
	$(AR) x ../aotus/LuaFortran/libflu.a ; \
	$(AR) x $(LOCAL__LUA_LIB) ; \
	$(AR) r ../libflookall.a *.o ; \
	cd ../ ; rm -rf .tmp_link )
	$(RANLIB) libflookall.a
# I have problems on unix to re-create the index table for the
# objects... Hence this will probably not work... You can
# try if you want to
#$(LIBTOOL) --mode=link --tag=FC $(AR) -o libflook.a src/libflook.a aotus/source/libaotus.a aotus/LuaFortran/libflu.a

.PHONY: doc
doc:
	doxygen doc/Doxyfile

.PHONY: test check
check: test
test: lib
	$(MAKE) -C test "ARCH_MAKE=$(ARCH_MAKE)" all

.PHONY: clean
clean:
	(cd aotus ; rm -f arch.make)
	-rm -rf doc/html doc/latex
	-rm -f libflookall.a
	-$(MAKE) -C aotus "ARCH_MAKE=$(ARCH_MAKE)" clean
	-$(MAKE) -C src "ARCH_MAKE=$(ARCH_MAKE)" clean
	-$(MAKE) -C test "ARCH_MAKE=$(ARCH_MAKE)" clean
