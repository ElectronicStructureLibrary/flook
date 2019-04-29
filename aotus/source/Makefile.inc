source_DIR = $(TOP_DIR)/source

# Append the source directory
VPATH := $(VPATH):$(source_DIR)

# Objects in this folder
source_OBJECTS = aot_err_module.o \
	       aot_fun_declaration_module.o \
	       aot_fun_module.o \
	       aot_out_general_module.o \
	       aot_out_module.o \
	       aot_path_module.o \
	       aot_references_module.o \
	       aot_table_module.o \
	       aot_table_ops_module.o \
	       aot_top_module.o \
	       aotus_module.o \
	       aot_vector_module.o
source_SOURCES := $(patsubst %.o, %.f90, $(source_OBJECTS))

# Add dependency for the LuaFortran objects
$(source_OBJECTS): $(LuaF_OBJECTS)

# Add to the global variables
SOURCES += $(source_SOURCES)
OBJECTS += $(source_OBJECTS)


# Local Variables:
#  mode: makefile-gmake
# End:

# Automatic creation of the dependencies may be achieved through:
#   sfmakedepend --depend=obj --modext=o --file=Makefile.inc *.f90

# DO NOT DELETE THIS LINE - used by make depend
aot_fun_module.o: aot_fun_declaration_module.o aot_references_module.o
aot_fun_module.o: aot_table_module.o aot_top_module.o
aot_out_module.o: aot_out_general_module.o
aot_path_module.o: aot_fun_module.o aot_table_module.o aotus_module.o
aot_references_module.o: aot_table_ops_module.o
aot_table_module.o: aot_err_module.o aot_table_ops_module.o aot_top_module.o
aot_table_ops_module.o: aot_top_module.o
aot_top_module.o: aot_err_module.o
aot_vector_module.o: aot_table_ops_module.o aot_top_module.o
aotus_module.o: aot_table_module.o aot_top_module.o aot_vector_module.o
