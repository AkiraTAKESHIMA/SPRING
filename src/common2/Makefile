TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude


OBJS = \
common_rt.o\
common_area_raster_polygon.o

all:
	$(MAKE) $(OBJS) ../common1/*.o

clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp*

.SUFFIXES: 
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FCFLAGS) -c $< -I../common1 -I$(MODDIR)

%.mod: %.f90
	$(MAKE) $(patsubst %.f90,%.o,%<)
