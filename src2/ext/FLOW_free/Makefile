TOPSRCDIR = ../..
include $(TOPSRCDIR)/Mkinclude

.SUFFIXES: .f90
%.o: %.f90
	$(FC) $(FCFLAGS) -c $< -I$(MODDIR)


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

TARGET1 = make_gcmmap
TARGET2 = modify_hires
TARGET3 = calc_uparea
TARGET4 = const_network
TARGET5 = define_catchment
TARGET6 = visual_check
TARGET7 = gcm_rivermap
TARGET8 = set_map
TARGET9 = calc_inpmat

.PHONY: all
all:
	make $(MODS)
	make $(TARGETS)

.PHONY: mods
mods:
	make $(MODS)

.PHONY: clean
clean:
	rm -f *~ $(TARGETS) *.o *.mod


$(TARGET1): $(TARGET1).o $(MODS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET2): $(TARGET2).o $(MODS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET3): $(TARGET3).o $(MODS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET4): $(TARGET4).o $(MODS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET5): $(TARGET5).o $(MODS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET6): $(TARGET6).o $(MODS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET7): $(TARGET7).o $(MODS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET8): $(TARGET8).o $(MODS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

$(TARGET9): $(TARGET9).o $(MODS) $(LIB)
	$(FC) $(FCFLAGS) -o $@ $^

