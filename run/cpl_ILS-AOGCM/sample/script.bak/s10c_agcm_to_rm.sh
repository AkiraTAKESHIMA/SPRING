#!/bin/bash
set -e

${python} s10c_agcm_to_rm.py\
  ../tmp/${name_step_01}/grdidx.bin\
  ../tmp/${name_step_06}/grdidx_river.bin\
  ../tmp/${name_step_10}/rt_agcm_to_rm
