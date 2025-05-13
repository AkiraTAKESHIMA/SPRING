TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(CMNDIR1) -I$(MODDIR)
%.o: %.F90
	$(FC) $(FCFLAGS) $(FOPT_USE_C) $(FOPT_TERMINAL_WIDTH) -c $< -I$(CMNDIR1) -I$(MODDIR)
%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I$(CMNDIR1) -I$(MODDIR)
%.mod: %.f90 %.o
	@:

TARGET = cmn2.a

OBJ_F = \
cmn2_type_rt.o\
cmn2_rt1d.o\
cmn2_area_raster_polygon.o\
cmn2_rt_base.o\
cmn2_rt_set.o\
cmn2_rt_stats.o\
cmn2_rt_error.o\
cmn2_rt_main_io.o\
cmn2_rt_main_util.o\
cmn2_rt_main_coef.o\
cmn2_rt_main_finish.o\
cmn2_rt_vrf_core.o\
cmn2_rt_vrf_driv.o\
cmn2_rt_vrf_io.o

OBJ_CMN = $(OBJ_F)

TESTPROGRAM = main
OBJ_TEST = main.o

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

$(TESTPROGRAM): $(OBJ_TEST) $(OBJ_CMN) $(CMNDIR1)/cmn1.a $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET) $(TESTPROGRAM)

.PHONY: cleantest
cleantest:
	$(RM) $(RMFLAGS) $(OBJ_TEST) $(TESTPROGRAM)
