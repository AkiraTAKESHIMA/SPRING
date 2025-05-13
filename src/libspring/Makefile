TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

SPRINGLIB = libspring.a

OBJ_F = \
ls_base.o\
ls_gs.o\
ls_rt.o\
ls_remap.o\
ls_manage.o\
ls.o

OBJS = $(OBJ_F)

all:
	$(MAKE) $(OBJ_F)
	$(MAKE) $(SPRINGLIB)

$(SPRINGLIB): $(OBJS) $(CMNDIR1)/cmn1.a $(CMNDIR2)/cmn2.a $(CMNDIR3)/cmn3.a $(LIB)
	$(RM) $(RMFLAGS) $@
	$(AR) cqT $@ $^
	printf "create $@\naddlib $@\nsave\nend\n" | ar -M

clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET)

.SUFFIXES: 
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(CMNDIR3) -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)

%.mod: %.f90
	$(MAKE) $(patsubst %.f90,%.o,%<)
