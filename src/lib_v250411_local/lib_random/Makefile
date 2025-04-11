TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(LIBDIR_CONST) -I$(LIBDIR_BASE)
%.mod: %.f90 %.o
	@:

TARGET = lib_random.a

OBJ_F = \
  lib_random_SFMT.o\
  lib_random_random.o\
  lib_random.o

OBJ_ALL = $(OBJ_F)

MOD_ALL = $(OBJ_F: .o=.mod)

.PHONY: all
all: $(TARGET)

$(TARGET): $(OBJ_ALL)
	$(AR) $(ARFLAGS) $@ $^
	$(RANLIB) $@

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod $(TARGET)
