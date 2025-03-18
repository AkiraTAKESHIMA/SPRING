TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude


OBJS = \
def_type.o\
mod_file.o\
mod_set.o\
mod_rt_latlon_latlon.o\
mod_rt_latlon_raster.o\
mod_rt_latlon_polygon.o\
mod_rt_raster_polygon.o\
mod_rt_polygon_polygon_regions.o\
mod_rt_polygon_polygon.o\
mod_rt.o\
mod_remap.o\
main.o

TARGET = main
BINDIR = $(TOPSRCDIR)/../bin/main_std
BIN = $(BINDIR)/remap.exe
PY = $(BINDIR)/remap_mkfig.py

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

install:
	$(MKDIR) $(MKDIRFLAGS) $(BINDIR)
	$(INSTALL) $(TARGET) $(BIN)
	$(CP) $(CPFLAGS) mkfig.py $(PY)

$(TARGET): $(OBJS) $(CMNDIR2)/*.o $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET) $(BIN) $(PY)

.SUFFIXES: 
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)

%.mod: %.f90
	$(MAKE) $(patsubst %.f90,%.o,%<)
