TOPSRCDIR = ../../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(LIBDIR_IO) -I$(LIBDIR_LOG) -I$(LIBDIR_BASE) -I$(LIBDIR_CONST)
%.mod: %.f90 %.o
	@:

TARGET = main

OBJ = main.o

.PHONY: all
all:
	$(MAKE) $(OBJ)
	$(MAKE) $(TARGET)

$(TARGET): $(OBJ) $(LIBDIR_IO)/*.a $(LIBDIR_ARRAY)/*.a $(LIBDIR_LOG)/*.a $(LIBDIR_TIME)/*.a $(LIBDIR_BASE)/*.a $(LIBDIR_CONST)/*.a
	$(FC) $(FCFLAGS) -o $@ $^

.PHONY: clean
clean:
	$(RM) $(RMFLAGS) *.o *.mod $(TARGET)
