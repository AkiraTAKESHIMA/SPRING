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
LIBDIR        = $(TOPSRCDIR)/lib
LIB           = $(LIBDIR)/lib.a
LIBDIR_CONST  = $(LIBDIR)/lib_const
LIBDIR_BASE   = $(LIBDIR)/lib_base
LIBDIR_TIME   = $(LIBDIR)/lib_time
LIBDIR_LOG    = $(LIBDIR)/lib_log
LIBDIR_UTIL   = $(LIBDIR)/lib_util
LIBDIR_RANDOM = $(LIBDIR)/lib_random
LIBDIR_ARRAY  = $(LIBDIR)/lib_array
LIBDIR_MATH   = $(LIBDIR)/lib_math
LIBDIR_STATS  = $(LIBDIR)/lib_stats
LIBDIR_PROJ   = $(LIBDIR)/lib_proj
LIBDIR_IO     = $(LIBDIR)/lib_io
LIBDIR_PNG    = $(LIBDIR)/lib_png
MODDIR = $(LIBDIR)/mod

CMNDIR = $(TOPSRCDIR)/common
CMNDIR1 = $(CMNDIR)/1
CMNDIR2 = $(CMNDIR)/2
CMNDIR3 = $(CMNDIR)/3

ifeq ($(ILS),T)
  include $(TOPSRCDIR)/../../../Mkinclude
  FCFLAGS = $(FFLAGS)
  CCFLAGS = $(CFLAGS)
endif
