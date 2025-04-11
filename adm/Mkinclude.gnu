#==============================================================
#
#==============================================================
DEBUG = true

#USE_C = false
#TERMINAL_WIDTH = 128

ILS ?= F
#==============================================================
#
#==============================================================
ifeq ($(USE_C), true)
  FOPT_USE_C = -DUSE_C
endif
ifdef TERMINAL_WIDTH
  FOPT_TERMINAL_WIDTH = -DTERMINAL_WIDTH=$(TERMINAL_WIDTH)
endif

FC = gfortran
CC = gcc
ifeq ($(DEBUG), true)
  FCFLAGS = -O0 -Wall -g\
            -fopenmp\
            -fcheck=bounds,do,mem,pointer,recursion\
            -frecord-marker=4\
            -fmax-errors=5\
            -ffpe-trap=invalid,zero,overflow,underflow
  CCFLAGS = -O0 -Wall -g
else
  FCFLAGS = -O2 -g
  CCFLAGS = -O2
endif
#==============================================================
#
#==============================================================
MAKE = make
INSTALL = install
AR = ar
ARFLAGS = rv
RANLIB = ranlib

MKDIR = mkdir
MKDIRFLAGS = -p
RM = rm
RMFLAGS = -f
CP = cp
CPFLAGS = -f

LD = $(FC)
LDFLAGS = $(FCFLAGS)
#==============================================================
#
#==============================================================
TOPLIBDIR     = $(TOPSRCDIR)/lib
LIB           = $(TOPLIBDIR)/lib.a
LIBDIR_CONST  = $(TOPLIBDIR)/lib_const
LIBDIR_BASE   = $(TOPLIBDIR)/lib_base
LIBDIR_TIME   = $(TOPLIBDIR)/lib_time
LIBDIR_LOG    = $(TOPLIBDIR)/lib_log
LIBDIR_UTIL   = $(TOPLIBDIR)/lib_util
LIBDIR_RANDOM = $(TOPLIBDIR)/lib_random
LIBDIR_ARRAY  = $(TOPLIBDIR)/lib_array
LIBDIR_MATH   = $(TOPLIBDIR)/lib_math
LIBDIR_STATS  = $(TOPLIBDIR)/lib_stats
LIBDIR_PROJ   = $(TOPLIBDIR)/lib_proj
LIBDIR_IO     = $(TOPLIBDIR)/lib_io
LIBDIR_PNG    = $(TOPLIBDIR)/lib_png
MODDIR = $(TOPLIBDIR)/mod

CMNDIR1 = $(TOPSRCDIR)/common1
CMNDIR2 = $(TOPSRCDIR)/common2
CMNDIR3 = $(TOPSRCDIR)/common3

ifeq ($(ILS),T)
  include $(TOPSRCDIR)/../../../Mkinclude
  FCFLAGS = $(FFLAGS)
  CCFLAGS = $(CFLAGS)
endif
