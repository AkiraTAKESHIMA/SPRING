#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./func_get_info_gs_latlon_uniform.sh
#===============================================================
#
#===============================================================
function write_conf_regrid_gs_latlon_uniform () {
  if [ $# -ne 1 ]; then
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    echo "Invalid number of arguments" >&2
    exit 1
  fi

  local gsName=${1}

  local nx
  local ny
  local west
  local east
  local south
  local north
  local is_south_to_north

  get_info_gs_latlon_uniform ${gsName}

  cat << EOF >> ${f_conf}

[grid_system_latlon]
  name: ${gsName}
  nx: ${nx}
  ny: ${ny}
  west: ${west}
  east: ${east}
  south: ${south}
  north: ${north}
  is_south_to_north: ${is_south_to_north}
[end]
EOF
}
