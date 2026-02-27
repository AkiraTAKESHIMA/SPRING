TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .c .f90
%.o: %.c
	$(CC) $(CCFLAGS) -c $<
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(LIBDIR_CONST)
%.mod: %.f90 %.o
	@:

OBJ_F = \
  lib_base_message.o\
  lib_base_char.o\
  lib_base.o

OBJ_C = \
  func_base_pointer.o\
  func_base_char.o

OBJ_ALL = $(OBJ_F) $(OBJ_C)

ifeq ($(FOPT_USE_C), -DUSE_C)
  OBJ_ALL = $(OBJ_F) $(OBJ_C)
else
  OBJ_ALL = $(OBJ_F)
endif

MOD_ALL = $(OBJ_F:.o=.mod)

.PHONY: all
all:
	$(MAKE) $(OBJ_ALL)

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod
