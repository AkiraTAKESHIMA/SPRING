#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./func_write_conf_regrid_report.sh
. ./func_write_conf_regrid_gs_lsm_latlon.sh
. ./func_write_conf_regrid_gs_latlon_uniform.sh
. ./func_write_conf_regrid_regridding.sh
. ./func_write_conf_regrid_options.sh
#===============================================================
#
#===============================================================
tgtMeshName=${io_rect_name}

opt_coef_sum_modify="undef"

for landType in "river" "noriv-real" "noriv-virt"; do
  f_conf="${dir_set_step}/make_rt_lsm-${landType}_to_${tgtMeshName}.conf"
  dir_tmp_this="${dir_tmp_step}/rt_lsm-${landType}_to_${tgtMeshName}"

  export f_conf

  rm -f ${f_conf} && touch ${f_conf}

  write_conf_regrid_report\
    ${dir_tmp_this}
  write_conf_regrid_gs_lsm_latlon\
    ${landType}
  write_conf_regrid_gs_latlon_uniform\
    ${tgtMeshName}
  write_conf_regrid_regridding\
    "target" ${opt_coef_sum_modify}\
    ${dir_tmp_this}\
    "index" "auto"
  write_conf_regrid_options

  ${exec_main_std_regrid} ${f_conf}
done

