TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(LIBDIR_CONST) -I$(LIBDIR_BASE) -I$(LIBDIR_TIME)
%.mod: %.f90 %.o
	@:

OBJ_ALL = \
  lib_log_proc.o\
  lib_log_proc2.o\
  lib_log_str.o\
  lib_log_array.o\
  lib_log.o

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod
