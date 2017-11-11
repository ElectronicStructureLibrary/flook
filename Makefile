# Define default VPATH
VPATH ?= $(shell pwd)

# the default target
.PHONY: default
default: lib


# SMEKASETTINGS (DO NOT DELETE)
# DO NOT CHANGE CONTENT IN THIS BLOCK
# IT MAY BE OVERWRITTEN WHEN REINSTALLING SMEKA
#
# This Makefile was created by smeka:
#  github.com/zerothi/smeka

# Top-directory of Makefile/source tree
# If need set, do so ABOVE this block!
TOP_DIR ?= .

# Directory of smeka default Makefiles
SMEKA_DIR = smeka

# Include the smeka settings!
include $(TOP_DIR)/$(SMEKA_DIR)/Makefile.smeka

# SMEKAENDSETTINGS (DO NOT DELETE)

ifdef PLATFORM
_LUA_PLATFORM := $(PLATFORM)
else
_LUA_PLATFORM := linux
endif


# Define internal aotus library build
.PHONY: prep-aotus lib-aotus clean-aotus
prep-aotus:
ifdef SETUP
	(mkdir -p aotus/obj ; cd aotus/obj ; \
		echo "TOP_DIR =../../$(TOP_DIR)/aotus" > Makefile ; \
		echo "PLATFORM = $(_LUA_PLATFORM)" >> Makefile ; \
		echo "include ../../$(TOP_DIR)/aotus/Makefile" >> Makefile )

else
	(mkdir -p aotus/obj ; cd aotus/obj ; \
		echo "TOP_DIR =../../$(TOP_DIR)/aotus" > Makefile ; \
		echo "PLATFORM = $(_LUA_PLATFORM)" >> Makefile ; \
		echo "include ../../$(TOP_DIR)/aotus/Makefile" >> Makefile )
endif
lib-aotus: prep-aotus
ifdef SETUP
	(cd aotus/obj ; make SETUP=../../$(SETUP) )
else
	(cd aotus/obj ; make )
endif
clean-aotus: prep-aotus
ifdef SETUP
	(cd aotus/obj ; make SETUP=../../$(SETUP) clean )
else
	(cd aotus/obj ; make clean )
endif


# Include the makefile in the source directories:
#    ./src
include $(TOP_DIR)/src/Makefile.inc


# The linker is a fortran compiler (otherwise we need -lgfortran when linking)
LINK := $(FC)


# Define targets for libraries
$(FLOOK_LIB_STATIC): $(OBJECTS)
$(FLOOK_LIB_SHARED): $(OBJECTS)
$(FLOOK_LIB_STATIC_ALL): $(OBJECTS)
.PHONY: lib
lib: $(LIBRARIES)


# Figure out if the aotus library is linked manually.
# In this case we do not add dependency on the 
ifeq (,$(findstring aotus,$(LIBS)))
 # Not found, we need to create correct dependency.
 $(OBJECTS): lib-aotus
 $(FLOOK_LIB_STATIC): | lib-aotus
 $(FLOOK_LIB_SHARED): | lib-aotus
 $(FLOOK_LIB_STATIC_ALL): | lib-aotus

# Add clean-target dependency
clean: clean-aotus

 # Now create the correct linker flags etc. for internal linking
 INCLUDES += -Iaotus/obj
endif


# Create target for the "full" library which has:
#  liblua.a libaotus.a libflu.a libflook.a
.PHONY: liball
liball: $(FLOOK_LIB_STATIC)
	(mkdir -p .tmp_link ; rm -f $(FLOOK_LIB_STATIC_ALL) ; cd .tmp_link ; \
	$(AR) x ../aotus/obj/external/lua-5.3.4/src/liblua.a ; \
	$(AR) x ../aotus/obj/libaotus.a ; \
	$(AR) x ../$(FLOOK_LIB_STATIC) ; \
	$(AR) r ../$(FLOOK_LIB_STATIC_ALL) *.o ; \
	cd ../ ; rm -rf .tmp_link)
	$(RANLIB) $(FLOOK_LIB_STATIC_ALL)

# Include the makefile in the test source directories:
#    ./src/test
include $(TOP_DIR)/src/test/Makefile.inc
