#==============================================================
#
#==============================================================
DEBUG = true

#USE_C = false
#TERMINAL_WIDTH = 128

ILS ?= T
ILS_SYS = Linux64-gnu-ompi
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

FCFLAGS_FAST  = -O2 -g
FCFLAGS_DEBUG = -O0 -Wall -g\
                -fcheck=bounds,do,mem,pointer,recursion\
                -frecord-marker=4\
                -fmax-errors=5\
                -ffpe-trap=invalid,zero,overflow,underflow

CC = gcc

CCFLAGS_FAST  = -O2
CCFLAGS_DEBUG = -O0 -Wall -g

ifeq ($(DEBUG),T)
  FCFLAGS = $(FCFLAGS_FAST)
  CCFLAGS = $(CCFLAGS_FAST)
else
  FCFLAGS = $(FCFLAGS_DEBUG)
  CCFLAGS = $(CCFLAGS_DEBUG)
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
  include $(TOPSRCDIR)/../../../sysdep/Makedef.$(ILS_SYS)
  TOPDIR = $(TOPSRCDIR)/../../../..
  FCFLAGS_FAST  = $(FFLAGS_FAST)
  FCFLAGS_DEBUG = $(FFLAGS_DEBUG)
  CCFLAGS_FAST  = $(CFLAGS_FAST)
  CCFLAGS_DEBUG = $(CFLAGS_DEBUG)
endif

