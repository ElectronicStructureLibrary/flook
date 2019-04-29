extdouble_DIR = $(TOP_DIR)/source/extdouble

# Append the source/extdouble directory
VPATH := $(VPATH):$(extdouble_DIR)

# Objects in this folder
ifneq ($(EXTDOUBLE),0)
extdouble_OBJECTS = \
	       aot_extdouble_fun_module.o \
	       aot_extdouble_out_module.o \
	       aot_extdouble_table_module.o \
	       aot_extdouble_top_module.o \
	       aot_extdouble_vector_module.o
aot_top_module.o: aot_extdouble_top_module.o
aot_out_module.o: aot_extdouble_out_module.o
aot_fun_module.o: aot_extdouble_fun_module.o
aot_table_module.o: aot_extdouble_table_module.o
aot_vector_module.o: aot_extdouble_vector_module.o
aot_extdouble_top_module.o: aot_err_module.o
else
extdouble_OBJECTS = \
	       dummy_extdouble_fun_module.o \
	       dummy_extdouble_out_module.o \
	       dummy_extdouble_table_module.o \
	       dummy_extdouble_top_module.o \
	       dummy_extdouble_vector_module.o
aot_top_module.o: dummy_extdouble_top_module.o
aot_out_module.o: dummy_extdouble_out_module.o
aot_fun_module.o: dummy_extdouble_fun_module.o
aot_table_module.o: dummy_extdouble_table_module.o
aot_vector_module.o: dummy_extdouble_vector_module.o
endif
extdouble_SOURCES := $(patsubst %.o, %.f90, $(extdouble_OBJECTS))

# Add dependency for the LuaFortran objects
$(extdouble_OBJECTS): $(LuaF_OBJECTS)

# Add to the global variables
SOURCES += $(extdouble_SOURCES)
OBJECTS += $(extdouble_OBJECTS)


# Local Variables:
#  mode: makefile-gmake
# End:

# Automatic creation of the dependencies may be achieved through:
#   sfmakedepend --depend=obj --modext=o --file=Makefile.inc *.f90

# DO NOT DELETE THIS LINE - used by make depend
aot_extdouble_fun_module.o: aot_extdouble_top_module.o
aot_extdouble_out_module.o: aot_extdouble_top_module.o
aot_extdouble_table_module.o: aot_extdouble_top_module.o
aot_extdouble_vector_module.o: aot_extdouble_top_module.o
