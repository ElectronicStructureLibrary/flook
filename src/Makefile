.SUFFIXES: .f90 .F90 .o .a

VPATH ?= $(shell pwd)

ARCH_MAKE_DEFAULT=$(VPATH)/../arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

# Setup the default things, in case they haven't been set.
AR      ?= ar
ARFLAGS ?= cru
RANLIB  ?= ranlib

ifdef AOTUS_DIR
INC += -I$(AOTUS_DIR)/include
AOTUS_LIBS = $(AOTUS_DIR)/lib/libaotus.a
AOTUS_LIBS+= $(AOTUS_DIR)/lib/libflu.a
else
AOTUS_DIR=$(VPATH)/../aotus
INC += -I$(AOTUS_DIR)/LuaFortran
INC += -I$(AOTUS_DIR)/source
AOTUS_LIBS = $(AOTUS_DIR)/source/libaotus.a
AOTUS_LIBS+= $(AOTUS_DIR)/LuaFortran/libflu.a
endif

# The different libraries
OBJS = fluaput.o

LIB = libfluaput.a

.PHONY: default
default: lib

.PHONY: lib
lib: $(LIB)

# Always force the lib to be re-linked
.PHONY: $(LIB)
$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS) $(AOTUS_LIBS)
	$(RANLIB) $(LIB)

.PHONY: clean
clean:
	-rm -f $(OBJS) $(LIB) *.s *.o *.mod 

dep:
	-if [ $(VPATH) = "." ] ; then \
	sfmakedepend --depend=obj --modext=o *.f90 *.F90 ; \
        else \
	sfmakedepend --depend=obj --modext=o $(VPATH)/*.f90 $(VPATH)/*.F90 ;\
        fi

# DO NOT DELETE THIS LINE - used by make depend
