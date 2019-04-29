LuaF_DIR = $(TOP_DIR)/LuaFortran

# Append the LuaFortran directory
VPATH := $(VPATH):$(LuaF_DIR)

# Objects in this folder
LuaF_OBJECTS = dump_lua_fif_module.o \
	     flu_binding.o \
	     flu_kinds_module.o \
	     lua_fif.o \
	     lua_parameters.o
LuaF_SOURCES := $(patsubst %.o, %.f90, $(LuaF_OBJECTS))

# Add the C-sources
LuaF_OBJECTS += wrap_lua_dump.o
LuaF_SOURCES += wrap_lua_dump.c


# Add to the global variables
SOURCES += $(LuaF_SOURCES)
OBJECTS += $(LuaF_OBJECTS)


# Local Variables:
#  mode: makefile-gmake
# End:

# Automatic creation of the dependencies may be achieved through:
#   sfmakedepend --depend=obj --modext=o --file=Makefile.inc *.f90

# DO NOT DELETE THIS LINE - used by make depend
flu_binding.o: dump_lua_fif_module.o flu_kinds_module.o lua_fif.o
flu_binding.o: lua_parameters.o
lua_fif.o: lua_parameters.o
