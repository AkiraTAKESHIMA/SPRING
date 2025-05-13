TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.o: %.F90
	$(FC) $(FCFLAGS) $(FOPT_USE_C) $(FOPT_TERMINAL_WIDTH) -c $< -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.o: %.c
	$(CC) $(CCFLAGS) -c $< -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.mod: %.f90 %.o
	@:

TARGET = cmn3.a

OBJ_F = \
cmn3_rt_llbnds.o\
cmn3_rt_latlon_latlon.o\
cmn3_rt_latlon_raster.o\
cmn3_rt_latlon_polygon.o\
cmn3_rt_raster_raster.o\
cmn3_rt_raster_polygon.o\
cmn3_rt_polygon_polygon_regions.o\
cmn3_rt_polygon_polygon.o\
cmn3_rt_driv.o\
cmn3_type_rst.o\
cmn3_rst_run.o

OBJ_CMN = $(OBJ_F)

OBJ_TEST = main.o
TESTPROGRAM = main

.PHONY: makemod
makemod:
	$(MAKE) $(OBJ_CMN)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJ_CMN)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

.PHONY: maketest
maketest:
	$(MAKE) $(OBJ_TEST)
	$(MAKE) $(TESTPROGRAM)

$(TESTPROGRAM): $(OBJ_TEST) $(OBJ_CMN) $(CMNDIR2)/cmn2.a $(CMNDIR1)/cmn1.a $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET) $(TESTPROGRAM)

.PHONY: cleantest
cleantest:
	$(RM) $(RMFLAGS) $(OBJ_TEST) $(TESTPROGRAM)
