#!/bin/bash
set -e

function write_conf_regrid_gs_ogcm () {
  if [ $# -ne 0 ]; then
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    echo "Invalid number of arguments" >&2
    exit 1
  fi

  cat << EOF >> ${f_conf}
[grid_system_polygon]
  name: "OGCM"
  nij: 92160
  np: 4
  dir: "${dir_spring}/dat/COCO/1deg"
  f_lon_vertex: "COCO_lon.bin"
  f_lat_vertex: "COCO_lat.bin"
  coord_unit: degree
  coord_miss: -999.d0
  arc_parallel: .true.
  fin_grdidx: "COCO_idx_ocean.bin"
  idx_miss: 0
[end]
EOF
}
