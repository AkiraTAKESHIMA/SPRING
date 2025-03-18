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

  srcMeshName="mat-${landType}"
  tgtMeshName="io-row"
  srcVrfFormat="index"
  tgtVrfFormat="auto"

  f_conf="../set/01_make_rt/${srcMeshName}_to_${tgtMeshName}.conf"
  rm -f ${f_conf} && touch ${f_conf}

  write_conf_remap_head\
    ${f_conf} ${srcMeshName} ${tgtMeshName}
  write_conf_remap_gs_mat_raster\
    ${f_conf} ${landType}
  write_conf_remap_gs_io\
    ${f_conf} ${tgtMeshName}\
    ${io_row_nx} ${io_row_ny}\
    ${io_row_west} ${io_row_east} ${io_row_south} ${io_row_north}\
    ${io_row_is_south_to_north}
  write_conf_remap_remapping\
    ${f_conf} ${srcMeshName} ${tgtMeshName}\
    ${srcVrfFormat} ${tgtVrfFormat}
  write_conf_remap_options\
    ${f_conf}

  ${exec_main_std_remap} ${f_conf}
done
