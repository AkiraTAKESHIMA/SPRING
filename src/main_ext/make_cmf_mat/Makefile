TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude


OBJS = \
def_const.o\
def_type.o\
mod_set.o\
mod_main.o\
main.o

TARGET = main
BINDIR = $(TOPSRCDIR)/../bin/ext
BIN = $(BINDIR)/make_cmf_mat.exe

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

install:
	$(MKDIR) $(MKDIRFLAGS) $(BINDIR)
	$(INSTALL) $(TARGET) $(BIN)

$(TARGET): $(OBJS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod core *~ *trace temp* $(TARGET) $(BIN)

.SUFFIXES: 
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(CMNDIR1) -I$(MODDIR)

%.mod: %.f90
	$(MAKE) $(patsubst %.f90,%.o,%<)
