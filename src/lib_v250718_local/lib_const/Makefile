TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $<
%.mod: %.f90 %.o
	@:

OBJ_F = \
  lib_const.o

OBJ_ALL = $(OBJ_F)

MOD_ALL = $(OBJ_F:.o=.mod)

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod
