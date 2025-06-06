#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./func_write_conf_remap_head.sh
. ./func_write_conf_remap_gs_io.sh
. ./func_write_conf_remap_gs_mat-latlon.sh
. ./func_write_conf_remap_gs_mat-raster.sh
. ./func_write_conf_remap_gs_cmf-latlon.sh
. ./func_write_conf_remap_remapping.sh
. ./func_write_conf_remap_options.sh
#===============================================================
#
#===============================================================
for landType in ${list_landType}; do

  srcMeshName="io-metnc"
  tgtMeshName="mat-${landType}"
  srcVrfFormat="auto"
  tgtVrfFormat="index"

  f_conf="../set/01_make_rt/${srcMeshName}_to_${tgtMeshName}.conf"
  rm -f ${f_conf} && touch ${f_conf}

  write_conf_remap_head\
    ${f_conf} ${srcMeshName} ${tgtMeshName}
  write_conf_remap_gs_io\
    ${f_conf} ${srcMeshName}\
    ${io_metnc_nx} ${io_metnc_ny}\
    ${io_metnc_west} ${io_metnc_east} ${io_metnc_south} ${io_metnc_north}\
    ${io_metnc_is_south_to_north}
  write_conf_remap_gs_mat_raster\
    ${f_conf} ${landType}
  write_conf_remap_remapping\
    ${f_conf} ${srcMeshName} ${tgtMeshName}\
    ${srcVrfFormat} ${tgtVrfFormat}
  write_conf_remap_options\
    ${f_conf}

  ${exec_main_std_remap} ${f_conf}
done
