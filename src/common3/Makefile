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

TARGET = common3.a

OBJ_F = \
common_rt_llbnds.o\
common_rt_latlon_latlon.o\
common_rt_latlon_raster.o\
common_rt_latlon_polygon.o\
common_rt_raster_raster.o\
common_rt_raster_polygon.o\
common_rt_polygon_polygon_regions.o\
common_rt_polygon_polygon.o\
common_rt_driv.o\
common_type_rst.o\
common_rst_run.o

OBJ_CMN = $(OBJ_F)

OBJ_TEST = main.o
TESTPROGRAM = main

.PHONY: makecommon
makecommon:
	$(MAKE) $(OBJ_CMN)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJ_CMN)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

.PHONY: maketest
maketest:
	$(MAKE) $(OBJ_TEST)
	$(MAKE) $(TESTPROGRAM)

$(TESTPROGRAM): $(OBJ_TEST) $(OBJ_CMN) $(CMNDIR2)/common2.a $(CMNDIR1)/common1.a $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET) $(TESTPROGRAM)

.PHONY: cleantest
cleantest:
	$(RM) $(RMFLAGS) $(OBJ_TEST) $(TESTPROGRAM)
