TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude


OBJS = \
common_const.o\
common_type.o\
common_file.o\
common_set.o\
common_gs.o

all:
	$(MAKE) $(OBJS)

clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp*

.SUFFIXES: 
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR)

%.mod: %.f90
	$(MAKE) $(patsubst %.f90,%.o,%<)
