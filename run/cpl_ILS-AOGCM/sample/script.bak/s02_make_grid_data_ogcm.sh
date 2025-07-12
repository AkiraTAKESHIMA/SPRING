#!/bin/bash
set -e
#set -x
#===============================================================
#
#===============================================================
dir_tmp_this="${dir_tmp}/${name_step_02}"
dir_set_this="${dir_set}/${name_step_02}"
f_conf="${dir_set_this}/make_grid_data_ogcm.conf"
#===============================================================
#
#===============================================================
mkdir -p ${dir_set_this}

cat << EOF > ${f_conf}
#
path_report: "${dir_tmp_this}/report.txt"

[grid_system_polygon]
  np : 4
  nij: 92160
  dir: "${dir_spring}/dat/COCO/1deg"
  f_lon_vertex: "COCO_lon.bin"
  f_lat_vertex: "COCO_lat.bin"
  coord_unit: degree
  coord_miss: -999.
  arc_parallel: .true.
  fin_grdidx: "COCO_idx_ocean.bin"
  idx_miss: 0

  out_form: index
  dir: "${dir_tmp_this}"
  fout_grdidx: "grdidx_ocean.bin"
  fout_grdara: "grdara_ocean.bin"
  fout_grdx  : "grdxyz_ocean.bin", rec=1
  fout_grdy  : "grdxyz_ocean.bin", rec=2
  fout_grdz  : "grdxyz_ocean.bin", rec=3
  fout_grdlon: "grdlonlat_ocean.bin", rec=1
  fout_grdlat: "grdlonlat_ocean.bin", rec=2
[end]

[options]
  old_files: remove

  earth_shape: sphere
  earth_r: ${earth_r}
[end]
EOF
#===============================================================
#
#===============================================================
${exec_main_std_make_grid_data} ${f_conf}
