quadruple_DIR = $(TOP_DIR)/source/quadruple

# Append the source/quadruple directory
VPATH := $(VPATH):$(quadruple_DIR)

# Objects in this folder
ifneq ($(QUADRUPLE),0)
quadruple_OBJECTS = \
	       aot_quadruple_fun_module.o \
	       aot_quadruple_out_module.o \
	       aot_quadruple_table_module.o \
	       aot_quadruple_top_module.o \
	       aot_quadruple_vector_module.o
aot_top_module.o: aot_quadruple_top_module.o
aot_out_module.o: aot_quadruple_out_module.o
aot_fun_module.o: aot_quadruple_fun_module.o
aot_table_module.o: aot_quadruple_table_module.o
aot_vector_module.o: aot_quadruple_vector_module.o
aot_quadruple_top_module.o: aot_err_module.o
else
quadruple_OBJECTS = \
	       dummy_quadruple_fun_module.o \
	       dummy_quadruple_out_module.o \
	       dummy_quadruple_table_module.o \
	       dummy_quadruple_top_module.o \
	       dummy_quadruple_vector_module.o
aot_top_module.o: dummy_quadruple_top_module.o
aot_out_module.o: dummy_quadruple_out_module.o
aot_fun_module.o: dummy_quadruple_fun_module.o
aot_table_module.o: dummy_quadruple_table_module.o
aot_vector_module.o: dummy_quadruple_vector_module.o
endif
quadruple_SOURCES := $(patsubst %.o, %.f90, $(quadruple_OBJECTS))

# Add dependency for the LuaFortran objects
$(quadruple_OBJECTS): $(LuaF_OBJECTS)

# Add to the global variables
SOURCES += $(quadruple_SOURCES)
OBJECTS += $(quadruple_OBJECTS)


# Local Variables:
#  mode: makefile-gmake
# End:


# Automatic creation of the dependencies may be achieved through:
#   sfmakedepend --depend=obj --modext=o --file=Makefile.inc aot*.f90

# DO NOT DELETE THIS LINE - used by make depend
aot_quadruple_fun_module.o: aot_quadruple_top_module.o
aot_quadruple_out_module.o: aot_quadruple_top_module.o
aot_quadruple_table_module.o: aot_quadruple_top_module.o
aot_quadruple_vector_module.o: aot_quadruple_top_module.o
