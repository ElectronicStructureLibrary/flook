
# Set the default VPATH
VPATH ?= $(shell pwd)

ARCH_MAKE_DEFAULT=$(VPATH)/arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

FLUAPUT_DIR = ../src
INC += -I$(FLUAPUT_DIR)
LIBS += $(FLUAPUT_DIR)/libfluaput.a

#####
# Define all local libraries needed
#####
ifdef AOTUS_DIR
INC += -I$(AOTUS_DIR)/include
AOTUS_LIBS = $(AOTUS_DIR)/lib/libaotus.a
AOTUS_LIBS+= $(AOTUS_DIR)/lib/libflu.a
else
AOTUS_DIR ?= ../aotus/source
INC += -I$(AOTUS_DIR)
FFLAGS += -L$(AOTUS_DIR)
LIBS += -laotus

LUAFORTRAN_DIR ?= ../aotus/LuaFortran
INC += -I$(LUAFORTRAN_DIR)
FFLAGS += -L$(LUAFORTRAN_DIR)
LIBS += -lflu
endif

ifdef LUA_DIR
LIBS += -L$(LUA_DIR)/lib $(LUA_LIB)
else
LIBS += -L../aotus/external/lua-5.3.0 -llua
endif

# Add dl library
LIBS += -ldl

.PHONY: all
all: aot_passreturn passreturn


.PHONY: passreturn
passreturn: passreturn.o
	$(FC) -o $@ $< $(FFLAGS) $(LIBS)
	-./$@

.PHONY: aot_passreturn
aot_passreturn: aot_passreturn.o
	$(FC) -o $@ $< $(FFLAGS) $(LIBS)
	-./$@

.PHONY: clean
clean:
	-rm -rf *.o *.mod 
	-rm -rf passreturn
