TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR)
%.o: %.F90
	$(FC) $(FCFLAGS) $(FOPT_USE_C) $(FOPT_TERMINAL_WIDTH) -c $< -I$(MODDIR)
%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I$(MODDIR)
%.mod: %.f90 %.o
	@:

TARGET = cmn1.a

OBJ_F = \
cmn1_const.o\
cmn1_const_util.o\
cmn1_type_opt.o\
cmn1_type_gs.o\
cmn1_file.o\
cmn1_set.o\
cmn1_opt_ctrl.o\
cmn1_opt_set.o\
cmn1_regions_base.o\
cmn1_gs_util.o\
cmn1_gs_grid_base.o\
cmn1_gs_base.o\
cmn1_gs_grid_util.o\
cmn1_gs_grid_core.o\
cmn1_gs_define_polygon.o\
cmn1_gs_define.o\
cmn1_gs_driv.o\
cmn1_gs_grid_io.o

OBJ_CMN = $(OBJ_F)

OBJ_TEST = main.o
TESTPROGRAM = main

.PHONY: makemod
makemod:
	$(MAKE) $(OBJ_F)
	$(MAKE) $(TARGET)

.PHONY: maketest
maketest:
	$(MAKE) $(OBJ_TEST)
	$(MAKE) $(TESTPROGRAM)

$(TARGET): $(OBJ_CMN)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

$(TESTPROGRAM): $(OBJ_TEST) $(OBJ_CMN) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET) $(TESTPROGRAM)

.PHONY: cleantest
cleantest:
	$(RM) $(RMFLAGS) $(OBJ_TEST) $(TESTPROGRAM)

