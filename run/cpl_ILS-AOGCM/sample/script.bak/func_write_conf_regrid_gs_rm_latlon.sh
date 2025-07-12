#!/bin/bash
set -e

function write_conf_regrid_gs_rm_latlon () {
  if [ $# -ne 1 ]; then
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    echo "Invalid number of arguments" >&2
    exit 1
  fi

  local landType=${1}

  cat << EOF >> ${f_conf}

[grid_system_latlon]
  nx: 720
  ny: 360
  west: -180
  east:  180
  south: -90
  north:  90
  is_south_to_north: .false.

  dir: "${dir_tmp}/${name_step_05}"
  fin_grdidx: "grdidx_${landType}.bin"
  idx_miss: -9999
[end]
EOF
}
