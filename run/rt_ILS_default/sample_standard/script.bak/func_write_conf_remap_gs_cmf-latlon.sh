#!/bin/bash
set -e
#===============================================================
#
#===============================================================
function write_conf_remap_gs_cmf_latlon () {
  local f_conf=${1}
  local landType=${2}

  if [ ${landType} == "river" ]; then
    f_grdidx=${cmf_lnf_grdidx_river}
  elif [ ${landType} == "noriv" ]; then
    f_grdidx=${cmf_lnf_grdidx_noriv}
  else
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    exit 1
  fi

cat << EOF >> ${f_conf}

[grid_system_latlon]
  name: "cmf-${landType}"
  nx: ${cmf_ncx}
  ny: ${cmf_ncy}
  west: ${cmf_west}
  east: ${cmf_east}
  south: ${cmf_south}
  north: ${cmf_north}
  is_south_to_north: ${cmf_is_south_to_north}
  fin_grdidx: "${f_grdidx}"
[end]
EOF
}
