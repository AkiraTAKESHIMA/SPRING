TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .o .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(CMNDIR3) -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.mod: %.f90 %.o
	@:

TARGET = libspring.a

OBJ_F = \
	ls_base.o\
	ls_gs.o\
	ls_rt.o\
	ls_remap.o\
	ls_manage.o\
	ls.o

OBJ_ALL = $(OBJ_F)

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJ_ALL) $(CMNDIR1)/*.o $(CMNDIR2)/*.o $(CMNDIR3)/*.o $(LIBDIR)/*/*.o
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET)

