TOPSRCDIR = ..
include $(TOPSRCDIR)/Mkinclude

TARGET = $(LIB)

OBJ =\
$(LIBDIR_CONST)/*.a\
$(LIBDIR_BASE)/*.a\
$(LIBDIR_TIME)/*.a\
$(LIBDIR_LOG)/*.a\
$(LIBDIR_UTIL)/*.a\
$(LIBDIR_ARRAY)/*.a\
$(LIBDIR_MATH)/*.a\
$(LIBDIR_STATS)/*.a\
$(LIBDIR_IO)/*.a\
$(LIBDIR_PROJ)/*.a

.PHONY: all
all: 
	$(MAKE) -C $(LIBDIR_CONST)
	$(MAKE) -C $(LIBDIR_BASE)
	$(MAKE) -C $(LIBDIR_TIME)
	$(MAKE) -C $(LIBDIR_LOG)
	$(MAKE) -C $(LIBDIR_UTIL)
	$(MAKE) -C $(LIBDIR_ARRAY)
	$(MAKE) -C $(LIBDIR_MATH)
	$(MAKE) -C $(LIBDIR_STATS)
	$(MAKE) -C $(LIBDIR_IO)
	$(MAKE) -C $(LIBDIR_PROJ)

	$(RM) $(RMFLAGS) $(MODDIR)/*.mod && $(MKDIR) $(MKDIRFLAGS) $(MODDIR)
	$(INSTALL) $(LIBDIR_CONST)/lib_const.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_BASE)/lib_base.mod     $(MODDIR)/.
	$(INSTALL) $(LIBDIR_TIME)/lib_time.mod     $(MODDIR)/.
	$(INSTALL) $(LIBDIR_LOG)/lib_log.mod       $(MODDIR)/.
	$(INSTALL) $(LIBDIR_UTIL)/lib_util.mod     $(MODDIR)/.
	$(INSTALL) $(LIBDIR_ARRAY)/lib_array.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_MATH)/lib_math.mod     $(MODDIR)/.
	$(INSTALL) $(LIBDIR_STATS)/lib_stats.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_IO)/lib_io.mod         $(MODDIR)/.
	$(INSTALL) $(LIBDIR_PROJ)/lib_proj.mod     $(MODDIR)/.

	$(MAKE) $(MKFLAGS) $(TARGET)

.PHONY: install
install:
	$(RM) $(RMFLAGS) $(MODDIR)/*.mod
	$(INSTALL) $(LIBDIR_CONST)/lib_const.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_BASE)/lib_base.mod     $(MODDIR)/.
	$(INSTALL) $(LIBDIR_TIME)/lib_time.mod     $(MODDIR)/.
	$(INSTALL) $(LIBDIR_LOG)/lib_log.mod       $(MODDIR)/.
	$(INSTALL) $(LIBDIR_UTIL)/lib_util.mod     $(MODDIR)/.
	$(INSTALL) $(LIBDIR_ARRAY)/lib_array.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_MATH)/lib_math.mod     $(MODDIR)/.
	$(INSTALL) $(LIBDIR_STATS)/lib_stats.mod   $(MODDIR)/.
	$(INSTALL) $(LIBDIR_IO)/lib_io.mod         $(MODDIR)/.
	$(INSTALL) $(LIBDIR_PROJ)/lib_proj.mod     $(MODDIR)/.

	$(MAKE) $(MKFLAGS) $(TARGET)


$(TARGET): $(OBJ)
	$(RM) $(RMFLAGS) $@
	$(AR) cqT $@ $^
	printf "create $@\naddlib $@\nsave\nend\n" | ar -M

.PHONY: clean
clean:
	$(MAKE) -C $(LIBDIR_CONST)  clean
	$(MAKE) -C $(LIBDIR_BASE)   clean
	$(MAKE) -C $(LIBDIR_TIME)   clean
	$(MAKE) -C $(LIBDIR_LOG)    clean
	$(MAKE) -C $(LIBDIR_UTIL)   clean
	$(MAKE) -C $(LIBDIR_ARRAY)  clean
	$(MAKE) -C $(LIBDIR_MATH)   clean
	$(MAKE) -C $(LIBDIR_STATS)  clean
	$(MAKE) -C $(LIBDIR_IO)     clean
	$(MAKE) -C $(LIBDIR_PROJ)   clean
	$(RM) $(RMFLAGS) $(MODDIR)/*.mod $(TARGET)
