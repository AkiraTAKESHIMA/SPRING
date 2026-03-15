TOPSRCDIR = ../../..
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

TARGET = main
BINDIR = $(TOPSRCDIR)/../bin/std
BIN = $(BINDIR)/remap.exe
PY = $(BINDIR)/mkfig_remap.py

OBJ_F = \
def_type.o\
mod_set.o\
mod_rt_latlon_latlon.o\
mod_rt_latlon_raster.o\
mod_rt_latlon_polygon.o\
mod_rt_raster_raster.o\
mod_rt_raster_polygon.o\
mod_rt_polygon_polygon.o\
mod_rt_driv.o\
mod_remap.o\
mod_finalize.o\
main.o

OBJ_ALL = $(OBJ_F)
OBJ_LIB = def_*.o mod_*.o

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJ_ALL) $(CMNDIR2)/*.o $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: install
install:
	$(MKDIR) $(MKDIRFLAGS) $(BINDIR)
	$(INSTALL) $(TARGET) $(BIN)
	$(CP) $(CPFLAGS) mkfig.py $(PY)

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET)

