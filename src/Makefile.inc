src_DIR = $(TOP_DIR)/src
# Append both this directory and iff preprocessed sources exist
VPATH := $(VPATH):$(src_DIR)


# Select the correct object depending on the object-orientation
ifeq ($(OO),1)
  src_OBJECTS = flook_f03.o
else
  src_OBJECTS = flook.o
endif

flook.inc:
	@$(ECHO) "#ifndef _FLOOK_INCLUDE_DEFINED" > $@
	@$(ECHO) "#define _FLOOK_INCLUDE_DEFINED" >> $@
	@$(ECHO) "#define _FLOOK_MAJOR_ $(PROJECT_MAJOR)" >> $@
	@$(ECHO) "#define _FLOOK_MINOR_ $(PROJECT_MINOR)" >> $@
	@$(ECHO) "#define _FLOOK_MICRO_ $(PROJECT_MICRO)" >> $@
	@$(ECHO) "#define _FLOOK_VERSION_ $(PROJECT_VERSION)" >> $@
	@$(ECHO) "#endif" >> $@

# Define objects that needs compilation
src_SOURCES = $(patsubst %.o, %.f90, $(src_OBJECTS))
# FPP_SOURCES are those sources that needs deletion upon make clean
src_FPP_SOURCES = $(src_SOURCES)
src_MODS = $(patsubst %.f90, %.mod, $(src_SOURCES))

SOURCES += $(src_SOURCES) flook.inc
FPP_SOURCES += $(src_FPP_SOURCES)
F_MODS  += $(src_MODS)
OBJECTS += $(src_OBJECTS)


.PHONY: clean-src
clean-src:
	-$(RM) $(RM_FLAG_FORCE) $(src_FPP_SOURCES)
	-$(RM) $(RM_FLAG_FORCE) $(src_OBJECTS)
	-$(RM) $(RM_FLAG_FORCE) *.mod flook.inc

clean: clean-src


# Local Variables:
#  mode: makefile-gmake
# End:
