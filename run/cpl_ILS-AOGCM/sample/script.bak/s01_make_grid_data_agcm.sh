#!/bin/bash
set -e
#set -x
#===============================================================
#
#===============================================================
dir_tmp_this="${dir_tmp}/${name_step_01}"
dir_set_this="${dir_set}/${name_step_01}"
f_conf="${dir_set_this}/make_grid_data_agcm.conf"
#===============================================================
#
#===============================================================
mkdir -p ${dir_set_this}

cat << EOF > ${f_conf}
#
path_report: "${dir_tmp_this}/report.txt"

[grid_system_latlon]
  nx: 256
  ny: 128

  dir: "${dir_spring}/dat/T85"
  f_lon_bound: "T85_lon_bound.bin"
  f_lat_bound: "T85_lat_bound.bin"
  is_south_to_north: .false.

  out_form: auto
  dir: "${dir_tmp_this}"
  fout_grdidx: "grdidx.bin"
  fout_grdara: "grdara.bin"
  fout_grdx  : "grdxyz.bin", rec=1
  fout_grdy  : "grdxyz.bin", rec=2
  fout_grdz  : "grdxyz.bin", rec=3
  fout_grdlon: "grdlonlat.bin", rec=1
  fout_grdlat: "grdlonlat.bin", rec=2
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
