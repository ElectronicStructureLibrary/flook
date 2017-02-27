test_src_DIR = $(TOP_DIR)/src/test
# Where to create the output of tests
test_out_DIR = test

src_TESTS += passreturn
t_src_OBJECTS += passreturn.o
$(test_out_DIR)/passreturn: $(test_COMMON_OBJECTS) $(test_out_DIR)/passreturn.o
	$(FC) -o $@ $^ -l$(FLOOK_LIB) $(FFLAGS) $(INCLUDES) $(LDFLAGS)
	@echo "#### TEST -- running: $@" >> run_tests.sh
	@echo $@ >> run_tests.sh

src_TESTS += tbl
t_src_OBJECTS += tbl.o
$(test_out_DIR)/tbl: $(test_COMMON_OBJECTS) $(test_out_DIR)/tbl.o
	$(FC) -o $@ $^ -l$(FLOOK_LIB) $(FFLAGS) $(INCLUDES) $(LDFLAGS)
	@echo "#### TEST -- running: $@" >> run_tests.sh
	@echo $@ >> run_tests.sh

src_TESTS += exp_flook
t_src_OBJECTS += exp_flook.o
$(test_out_DIR)/exp_flook: $(test_COMMON_OBJECTS) $(test_out_DIR)/exp_flook.o
	$(FC) -o $@ $^ -l$(FLOOK_LIB) $(FFLAGS) $(INCLUDES) $(LDFLAGS)
	@echo "#### TEST -- running: $@" >> run_tests.sh
	@echo $@ >> run_tests.sh

src_TESTS += aot_passreturn
t_src_OBJECTS += aot_passreturn.o
$(test_out_DIR)/aot_passreturn: $(test_COMMON_OBJECTS) $(test_out_DIR)/aot_passreturn.o
	$(FC) -o $@ $^ -l$(FLOOK_LIB) $(FFLAGS) $(INCLUDES) $(LDFLAGS)
	@echo "#### TEST -- running: $@" >> run_tests.sh
	@echo $@ >> run_tests.sh


# Local Variables:
#  mode: makefile-gmake
# End:
