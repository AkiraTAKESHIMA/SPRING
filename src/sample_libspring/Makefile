TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude


OBJS = \
main.o

TARGET = main

LIBSPRINGDIR=$(TOPSRCDIR)/libspring

all:
	$(MAKE) $(TARGET)

$(TARGET): $(OBJS) $(LIBSPRINGDIR)/libspring.a
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET)

.SUFFIXES: 
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(LIBSPRINGDIR)

%.mod: %.f90
	$(MAKE) $(patsubst %.f90,%.o,%<)

