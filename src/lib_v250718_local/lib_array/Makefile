TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90 .o .mod
%.o: %.f90
	$(FC) $(FCFLAGS) -c -o $@ $< -I$(LIBDIR_CONST) -I$(LIBDIR_BASE) -I$(LIBDIR_LOG)
%.mod: %.f90 %.o
	@:

OBJ_F = \
  lib_array_realloc.o\
  lib_array_copy.o\
  lib_array_reverse.o\
  lib_array_sort.o\
  lib_array.o

OBJ_ALL = $(OBJ_F)

MOD_ALL = $(OBJ_F:.o=.mod)

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod
