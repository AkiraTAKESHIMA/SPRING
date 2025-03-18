#!/bin/bash
set -e
#===============================================================
#
#===============================================================
function write_conf_remap_gs_mat_latlon () {
  local f_conf=${1}
  local landType=${2}

  if [ ${landType} == "river" ]; then
    f_grdidx=${mat_lnf_grdidx_river}
  elif [ ${landType} == "noriv" ]; then
    f_grdidx=${mat_lnf_grdidx_noriv}
  else
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    exit 1
  fi

cat << EOF >> ${f_conf}

[grid_system_latlon]
  name: "mat-${landType}"
  nx: ${mat_ncx}
  ny: ${mat_ncy}
  west: ${mat_west}
  east: ${mat_east}
  south: ${mat_south}
  north: ${mat_north}
  is_south_to_north: ${mat_is_south_to_north}
  fin_grdidx: "${f_grdidx}"
[end]
EOF
}
