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
# Check SMEKA_uname_S for platform
ifeq ($(SMEKA_uname_S),Linux)
_LUA_PLATFORM := linux
endif
ifeq ($(SMEKA_uname_S),Darwin)
_LUA_PLATFORM := macosx
endif
ifeq ($(SMEKA_uname_S),FreeBSD)
_LUA_PLATFORM := freebsd
endif
ifeq ($(SMEKA_uname_S),OpenBSD)
_LUA_PLATFORM := bsd
endif
ifeq ($(SMEKA_uname_S),SunOS)
_LUA_PLATFORM := solaris
endif
endif


# Define internal aotus library build
.PHONY: prep-aotus lib-aotus clean-aotus
prep-aotus:
ifdef SETUP
	($(MKDIR) $(MKDIR_FLAG_PARENT) aotus/obj ; cd aotus/obj ; \
		$(ECHO) "TOP_DIR =../../$(TOP_DIR)/aotus" > Makefile ; \
		$(ECHO) "PLATFORM = $(_LUA_PLATFORM)" >> Makefile ; \
		$(ECHO) "include ../../$(TOP_DIR)/aotus/Makefile.smeka" >> Makefile )

else
	($(MKDIR) $(MKDIR_FLAG_PARENT) aotus/obj ; cd aotus/obj ; \
		$(ECHO) "TOP_DIR =../../$(TOP_DIR)/aotus" > Makefile ; \
		$(ECHO) "PLATFORM = $(_LUA_PLATFORM)" >> Makefile ; \
		$(ECHO) "include ../../$(TOP_DIR)/aotus/Makefile.smeka" >> Makefile )
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
lib: flook.inc $(LIBRARIES)


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
	($(MKDIR) $(MKDIR_FLAG_PARENT) .tmp_link ; $(RM) $(RM_FLAG_FORCE) $(FLOOK_LIB_STATIC_ALL) ; cd .tmp_link ; \
	$(AR) x ../aotus/obj/external/lua-5.3.5/src/liblua.a ; \
	$(AR) x ../aotus/obj/libaotus.a ; \
	$(AR) x ../$(FLOOK_LIB_STATIC) ; \
	$(AR) r ../$(FLOOK_LIB_STATIC_ALL) *.o ; \
	cd ../ ; $(RM) $(RM_FLAG_FORCE) $(RM_FLAG_RECURSE) .tmp_link)
	$(RANLIB) $(FLOOK_LIB_STATIC_ALL)

# Include the makefile in the test source directories:
#    ./src/test
include $(TOP_DIR)/src/test/Makefile.inc
