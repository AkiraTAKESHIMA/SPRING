#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./func_write_conf_regrid_report.sh
. ./func_write_conf_regrid_gs_latlon_uniform.sh
. ./func_write_conf_regrid_gs_lsm_latlon.sh
. ./func_write_conf_regrid_regridding.sh
. ./func_write_conf_regrid_options.sh
#===============================================================
#
#===============================================================
srcMeshName=${io_bnd_name}

opt_coef_sum_modify="undef"

for landType in "river" "noriv"; do
  f_conf="${dir_set_step}/make_rt_${srcMeshName}_to_lsm-${landType}.conf"
  dir_tmp_this="${dir_tmp_step}/rt_${srcMeshName}_to_lsm-${landType}"

  export f_conf

  rm -f ${f_conf} && touch ${f_conf}

  write_conf_regrid_report\
    ${dir_tmp_this}
  write_conf_regrid_gs_latlon_uniform\
    ${srcMeshName}
  write_conf_regrid_gs_lsm_latlon\
    ${landType}
  write_conf_regrid_regridding\
    "target" ${opt_coef_sum_modify}\
    ${dir_tmp_this}\
    "none" "none"
  write_conf_regrid_options

  ${exec_main_std_regrid} ${f_conf}
done
