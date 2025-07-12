#!/bin/bash
set -e

function write_conf_regrid_gs_agcm () {
  if [ $# -ne 0 ]; then
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    echo "Invalid number of arguments" >&2
    exit 1
  fi

  cat << EOF >> ${f_conf}
[grid_system_latlon]
  name: "AGCM"
  nx: 256
  ny: 128
  dir: "${dir_spring}/dat/T85"
  f_lon_bound: "T85_lon_bound.bin"
  f_lat_bound: "T85_lat_bound.bin"
  is_south_to_north: .false.
[end]
EOF
}
