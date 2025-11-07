TOPSRCDIR = ../../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90 .F90 .c
%.o: %.f90
	$(FC) $(FCFLAGS) -c $<\
 -I$(CMNDIR3) -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.o: %.F90
	$(FC) $(FCFLAGS) $(FOPT_USE_C) $(FOPT_TERMINAL_WIDTH) -c $<\
 -I$(CMNDIR3) -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.o: %.c
	$(CC) $(CCFLAGS) -c $<\
 -I$(CMNDIR3) -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)
%.mod: %.f90 %.o
	@:

OBJS = \
def_type.o\
mod_set.o\
mod_main.o\
mod_finalize.o\
main.o

TARGET = main
BINDIR = $(TOPSRCDIR)/../bin/std
BIN = $(BINDIR)/make_grid_data.exe

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

install:
	$(MKDIR) $(MKDIRFLAGS) $(BINDIR)
	$(INSTALL) $(TARGET) $(BIN)

$(TARGET): $(OBJS) $(CMNDIR3)/*.o $(CMNDIR2)/*.o $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET)

