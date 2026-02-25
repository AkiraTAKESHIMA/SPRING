TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(LIBDIR_CONST)
%.mod: %.f90 %.o
	@:

OBJ_F = \
  lib_proj_sinusoidal.o\
  lib_proj_lambertconformalconic.o

OBJ_ALL = $(OBJ_F)

MOD_ALL = $(OBJ_ALL:.o=.mod)

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod
