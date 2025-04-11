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

TARGET = common1.a

OBJ_F = \
common_const.o\
common_type_opt.o\
common_type_gs.o\
common_file.o\
common_set.o\
common_opt_set.o\
common_gs_util.o\
common_gs_zone.o\
common_gs_grid_base.o\
common_gs_base.o\
common_gs_grid_std.o\
common_gs_grid_util.o\
common_gs_grid_io.o\
common_gs_grid_core.o\
common_gs_define_polygon.o\
common_gs_define.o\
common_gs_grid_driv.o

OBJ_CMN = $(OBJ_F)

OBJ_TEST = main.o
TESTPROGRAM = main

.PHONY: makecommon
makecommon:
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

