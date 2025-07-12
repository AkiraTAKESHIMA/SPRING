#!/bin/bash
set -e
set -x
#===============================================================
#
#===============================================================
dir_tmp_step="${dir_tmp}/${name_step_06}"
dir_set_step="${dir_set}/${name_step_06}"

mkdir -p ${dir_set_step}
#===============================================================
#
#===============================================================
for landType in "river" "river-end" "noriv" "ocean"; do
  f_conf="${dir_set_step}/make_grid_data_rm_${landType}.conf"
  dir_tmp_this="${dir_tmp_step}"

  cat << EOF > ${f_conf}
#
path_report: "${dir_tmp_this}/report_${landType}.txt"

[grid_system_raster]
  nx: 21600
  ny: 10800
  west: -180
  east:  180
  south: -90
  north:  90
  is_south_to_north: .false.

  dir: "${dir_tmp}/${name_step_05}"
  fin_rstidx: "rstidx_${landType}.bin"
  fin_grdidx: "grdidx_${landType}.bin"
  in_grid_sz: 720, 360

  out_form: index
  dir: "${dir_tmp_this}"
  fout_grdidx: "grdidx_${landType}.bin"
  fout_grdara: "grdara_${landType}.bin"
  fout_grdx  : "grdxyz_${landType}.bin", rec=1
  fout_grdy  : "grdxyz_${landType}.bin", rec=2
  fout_grdz  : "grdxyz_${landType}.bin", rec=3
  fout_grdlon: "grdlonlat_${landType}.bin", rec=1
  fout_grdlat: "grdlonlat_${landType}.bin", rec=2

  idx_miss   : -9999
  ara_miss   : -1d20
  xyz_miss   : -1d20
  lonlat_miss: -1d20
[end]

[options]
  old_files: remove

  earth_shape: sphere
  earth_r: ${earth_r}
[end]
EOF

  ${exec_main_std_make_grid_data} ${f_conf}

  ln -nsf ../${name_step_05}/rstidx_${landType}.bin ../tmp/${name_step_06}/.
done
