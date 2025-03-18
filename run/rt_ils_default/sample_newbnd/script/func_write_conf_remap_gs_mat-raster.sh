#!/bin/bash
set -e
#===============================================================
#
#===============================================================
function write_conf_remap_gs_mat_raster () {
  local f_conf=${1}
  local landType=${2}

  if [ ${landType} == "river" ]; then
    f_grdidx=${mat_lnf_grdidx_river}
    f_rstidx=${mat_lnf_rstidx_river}
  elif [ ${landType} == "noriv" ]; then
    f_grdidx=${mat_lnf_grdidx_noriv}
    f_rstidx=${mat_lnf_rstidx_noriv}
  else
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    exit 1
  fi

cat << EOF >> ${f_conf}

[grid_system_raster]
  name: "mat-${landType}"
  nx: ${mat_ndx}
  ny: ${mat_ndy}
  west: ${mat_west}
  east: ${mat_east}
  south: ${mat_south}
  north: ${mat_north}
  fin_rstidx: "${f_rstidx}"
  fin_grdidx: "${f_grdidx}"
  in_grid_sz: ${mat_ncx}, ${mat_ncy}
  is_south_to_north: ${mat_is_south_to_north}
[end]
EOF
}
