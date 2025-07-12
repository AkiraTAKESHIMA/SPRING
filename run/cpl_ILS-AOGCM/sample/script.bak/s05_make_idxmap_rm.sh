#!/bin/bash
set -e
#set -x
#===============================================================
#
#===============================================================
dir_tmp_this="${dir_tmp}/${name_step_05}"
dir_set_this="${dir_set}/${name_step_05}"
f_conf="${dir_set_this}/make_idxmap_rm.conf"
#===============================================================
#
#===============================================================
mkdir -p ${dir_set_this}

cat << EOF > ${f_conf}
#

[common]
  nx_grid: 720
  ny_grid: 360
  nx_raster: 21600
  ny_raster: 10800
[end]

[cama-flood]
  dir: "${dir_tmp}/${name_step_04}/map"
  fin_catmxy: "1min/catmxy.bin"
  fin_nextxy: "nextxy.bin"

  dir: "${dir_tmp_this}"
  fout_grdidx_river    : "grdidx_river.bin"
  fout_grdidx_river_end: "grdidx_river-end.bin"
  fout_grdidx_noriv    : "grdidx_noriv.bin"
  fout_grdidx_ocean    : "grdidx_ocean.bin"
  fout_rstidx_river    : "rstidx_river.bin"
  fout_rstidx_river_end: "rstidx_river-end.bin"
  fout_rstidx_noriv    : "rstidx_noriv.bin"
  fout_rstidx_ocean    : "rstidx_ocean.bin"

  catmxy_noriv_coastal: 0
  catmxy_noriv_inland : -1
  catmxy_ocean        : -9999
  nextxy_river_mouth  : -9
  nextxy_river_inland : -10
  nextxy_ocean        : -9999
  idx_miss: -9999
[end]

[options]
  old_files: remove
[end]
EOF
#===============================================================
#
#===============================================================
${exec_main_ext_make_cmf_mat} ${f_conf}
