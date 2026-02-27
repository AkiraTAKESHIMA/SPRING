TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude


OBJ1 = test_remap_latlon_latlon.o
OBJ2 = test_remap_raster_raster.o

TARGET1 = test_remap_latlon_latlon
TARGET2 = test_remap_raster_raster
TARGETALL = $(TARGET1) $(TARGET2)

LIBSPRINGDIR=$(TOPSRCDIR)/libspring

all:
	$(MAKE) $(TARGET1)
	$(MAKE) $(TARGET2)

$(TARGET1): $(OBJ1) $(LIBSPRINGDIR)/libspring.a
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET2): $(OBJ2) $(LIBSPRINGDIR)/libspring.a
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGETALL)

.SUFFIXES: 
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(LIBSPRINGDIR)

%.mod: %.f90
	$(MAKE) $(patsubst %.f90,%.o,%<)

