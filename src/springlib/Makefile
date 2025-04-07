TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

SPRINGLIB = springlib.a
TARGETLIB = mod.a

OBJ_F = \
slib_base.o\
slib_gs.o\
slib_remap.o\
springlib.o

OBJDIR1 = $(TOPSRCDIR)/main_std/remap

all:
	$(MAKE) $(OBJ_F)
	$(MAKE) $(TARGETLIB)
	$(MAKE) $(SPRINGLIB)

$(TARGETLIB): $(OBJ_F)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

$(SPRINGLIB): $(TARGETLIB) $(OBJDIR1)/remap.a $(CMNDIR1)/common1.a $(CMNDIR2)/common2.a $(LIB)
	$(RM) $(RMFLAGS) $@
	$(AR) cqT $@ $^
	printf "create $@\naddlib $@\nsave\nend\n" | ar -M

clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET)

.SUFFIXES: 
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(OBJDIR1) -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)

%.mod: %.f90
	$(MAKE) $(patsubst %.f90,%.o,%<)
