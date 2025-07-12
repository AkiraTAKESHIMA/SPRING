#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./func_write_conf_regrid_report.sh
. ./func_write_conf_regrid_gs_ogcm.sh
. ./func_write_conf_regrid_gs_agcm.sh
. ./func_write_conf_regrid_regridding.sh
. ./func_write_conf_regrid_options.sh
#===============================================================
#
#===============================================================
opt_coef_sum_modify="1.d0"

f_conf="${dir_set_step}/make_rt_ogcm_to_agcm.conf"
dir_tmp_this="${dir_tmp_step}/rt_ogcm_to_agcm"

export f_conf

rm -f ${f_conf} && touch ${f_conf}

write_conf_regrid_report\
  ${dir_tmp_this}
write_conf_regrid_gs_ogcm
write_conf_regrid_gs_agcm
write_conf_regrid_regridding\
  "target" ${opt_coef_sum_modify}\
  ${dir_tmp_this}\
  "index" "auto"
write_conf_regrid_options

${exec_main_std_regrid} ${f_conf}
