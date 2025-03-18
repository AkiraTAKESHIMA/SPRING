TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude


OBJS = \
def_type.o\
mod_set.o\
mod_data.o\
mod_rasterize_latlon.o\
mod_rasterize_polygon.o\
mod_main.o\
main.o

TARGET = main
BINDIR = $(TOPSRCDIR)/../bin/main_std
BIN = $(BINDIR)/rasterize.exe

all:
	$(MAKE) $(OBJS)
	$(MAKE) $(TARGET)

install:
	$(MKDIR) $(MKDIRFLAGS) $(BINDIR)
	$(INSTALL) $(TARGET) $(BIN)

$(TARGET): $(OBJS) $(CMNDIR2)/*.o $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

clean:
	$(RM) $(RMFLAGS) *.o *.mod *.a core *~ *trace temp* $(TARGET) $(BIN)

.SUFFIXES: 
.SUFFIXES: .o .f90 .mod

.f90.o:
	$(FC) $(FCFLAGS) -c $< -I$(CMNDIR2) -I$(CMNDIR1) -I$(MODDIR)

%.mod: %.f90
	$(MAKE) $(patsubst %.f90,%.o,%<)
