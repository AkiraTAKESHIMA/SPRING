#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./func_write_conf_remap_head.sh
. ./func_write_conf_remap_gs_io.sh
. ./func_write_conf_remap_gs_mat-latlon.sh
. ./func_write_conf_remap_gs_mat-raster.sh
. ./func_write_conf_remap_remapping.sh
. ./func_write_conf_remap_options.sh
#===============================================================
#
#===============================================================
layer=0
for landType in ${list_landType}; do
  layer=$(( layer + 1 ))

  srcMeshName="io-bnd"
  tgtMeshName="mat-${landType}"
  srcVrfFormat="auto"
  tgtVrfFormat="index"

  f_conf="../set/01_make_rt/${srcMeshName}_to_${tgtMeshName}.conf"
  rm -f ${f_conf} && touch ${f_conf}

  write_conf_remap_head\
    ${f_conf} ${srcMeshName} ${tgtMeshName}
  write_conf_remap_gs_io\
    ${f_conf} ${srcMeshName}\
    ${io_bnd_nx} ${io_bnd_ny}\
    ${io_bnd_west} ${io_bnd_east} ${io_bnd_south} ${io_bnd_north}\
    ${io_bnd_is_south_to_north}\
    ${layer}
  write_conf_remap_gs_mat_latlon\
    ${f_conf} ${landType}
  write_conf_remap_remapping\
    ${f_conf} ${srcMeshName} ${tgtMeshName}\
    ${srcVrfFormat} ${tgtVrfFormat}
  write_conf_remap_options\
    ${f_conf}

  ${exec_main_std_remap} ${f_conf}
done
