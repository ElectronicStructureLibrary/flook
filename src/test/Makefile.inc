test_src_DIR = $(TOP_DIR)/src/test
# Where to create the output of tests
VPATH := $(VPATH):$(test_src_DIR)


src_TESTS += tst_passreturn
t_src_OBJECTS += tst_passreturn.o
tst_passreturn: $(test_COMMON_OBJECTS) tst_passreturn.o
	$(FC) -o $@ $^ -L. -l$(FLOOK_LIB)all $(FFLAGS) $(INCLUDES) $(LDFLAGS)
	@echo "#### TEST -- running: $@" >> run_tests.sh
	@echo "cp $(test_src_DIR)/tst_passreturn.lua ." >> run_tests.sh
	@echo "./$@" >> run_tests.sh

src_TESTS += tst_tbl
t_src_OBJECTS += tst_tbl.o
tst_tbl: $(test_COMMON_OBJECTS) tst_tbl.o
	$(FC) -o $@ $^ -L. -l$(FLOOK_LIB)all $(FFLAGS) $(INCLUDES) $(LDFLAGS)
	@echo "#### TEST -- running: $@" >> run_tests.sh
	@echo "cp $(test_src_DIR)/tst_tbl.lua ." >> run_tests.sh
	@echo "./$@" >> run_tests.sh

src_TESTS += tst_exp_flook
t_src_OBJECTS += tst_exp_flook.o
tst_exp_flook: $(test_COMMON_OBJECTS) tst_exp_flook.o
	$(FC) -o $@ $^ -L. -l$(FLOOK_LIB)all $(FFLAGS) $(INCLUDES) $(LDFLAGS)
	@echo "#### TEST -- running: $@" >> run_tests.sh
	@echo "cp $(test_src_DIR)/tst_exp_flook.lua ." >> run_tests.sh
	@echo "./$@" >> run_tests.sh

src_TESTS += tst_aot_passreturn
t_src_OBJECTS += tst_aot_passreturn.o
tst_aot_passreturn: $(test_COMMON_OBJECTS) tst_aot_passreturn.o
	$(FC) -o $@ $^ -L. -l$(FLOOK_LIB)all $(FFLAGS) $(INCLUDES) $(LDFLAGS)
	@echo "#### TEST -- running: $@" >> run_tests.sh
	@echo "./$@" >> run_tests.sh

.PHONY: clean-test
clean-test:
	-rm -f $(src_TESTS) $(t_src_OBJECTS) run_tests.sh

# Add clean-test to target
clean: clean-test

$(t_src_OBJECTS): | liball
$(src_TESTS): | liball
.PHONY: test check
test: $(src_TESTS)
	sh ./run_tests.sh
check: test

# Local Variables:
#  mode: makefile-gmake
# End:
