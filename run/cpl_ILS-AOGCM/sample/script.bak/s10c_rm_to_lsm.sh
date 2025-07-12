#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./func_write_conf_regrid_report.sh
. ./func_write_conf_regrid_gs_rm_latlon.sh
. ./func_write_conf_regrid_gs_lsm_latlon.sh
. ./func_write_conf_regrid_regridding.sh
. ./func_write_conf_regrid_options.sh
#===============================================================
#
#===============================================================
opt_coef_sum_modify="undef"

f_conf="${dir_set_step}/make_rt_rm_to_lsm.conf"
dir_tmp_this="${dir_tmp_step}/rt_rm_to_lsm"

export f_conf

rm -f ${f_conf} && touch ${f_conf}

write_conf_regrid_report\
  ${dir_tmp_this}
write_conf_regrid_gs_rm_latlon\
  "river"
write_conf_regrid_gs_lsm_latlon\
  "river"
write_conf_regrid_regridding\
  "target" ${opt_coef_sum_modify}\
  ${dir_tmp_this}\
  "none" "none"
write_conf_regrid_options

${exec_main_std_regrid} ${f_conf}
