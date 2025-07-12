#!/bin/bash
set -e
#===============================================================
#
#===============================================================
dir_tmp_this="${dir_tmp}/${name_step_03}"
dir_set_this="${dir_set}/${name_step_03}"
f_conf="${dir_set_this}/rasterize_ogcm-land.conf"
#===============================================================
#
#===============================================================
mkdir -p ${dir_set_this}

cat << EOF > ${f_conf}
#
path_report: "${dir_tmp_this}/report.txt"

[grid_system_polygon]
  np: 4
  nij: 92160
  dir: "${dir_spring}/dat/COCO/1deg"
  f_lon_vertex: "COCO_lon.bin"
  f_lat_vertex: "COCO_lat.bin"
  coord_unit: degree
  coord_miss: -999.d0
  arc_parallel: .true.
  fin_grdidx: "COCO_idx_land.bin"
  idx_miss: 0
[end]

[raster]
  nx: 21600
  ny: 10800
  west: -180
  east:  180
  south: -90
  north:  90
  is_south_to_north: .false.
[end]

[output]
  include_min: .false.
  frac_min: ${lndfrc_min}

  dir: "${dir_tmp_this}"
  f_area_sum: "area.bin"
  f_frac_sum: "frac.bin"
  f_mask    : "mask.bin", int1
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
${exec_main_std_rasterize} ${f_conf}
