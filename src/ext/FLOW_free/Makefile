TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES:
.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR) -I$(CMNDIR1)


MODS = \
	def_consts.o\
	mod_utils.o

TARGETS = \
	$(TARGET1)\
	$(TARGET2)\
	$(TARGET3)\
	$(TARGET4)\
	$(TARGET5)\
	$(TARGET6)\
	$(TARGET7)\
	$(TARGET8)\
	$(TARGET9)

TARGET1 = make_gcmmap.exe
TARGET2 = modify_hires.exe
TARGET3 = calc_uparea.exe
TARGET4 = const_network.exe
TARGET5 = define_catchment.exe
TARGET6 = visual_check.exe
TARGET7 = gcm_rivermap.exe
TARGET8 = set_map.exe
TARGET9 = calc_inpmat.exe

.PHONY: all
all:
	make $(MODS)
	make $(TARGETS)

.PHONY: mods
mods:
	make $(MODS)

.PHONY: clean
clean:
	rm -f *~ *.o *.mod *.exe


$(TARGET1): $(TARGET1).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET2): $(TARGET2).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET3): $(TARGET3).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET4): $(TARGET4).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET5): $(TARGET5).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET6): $(TARGET6).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET7): $(TARGET7).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET8): $(TARGET8).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET9): $(TARGET9).o $(MODS) $(CMNDIR1)/*.o $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

