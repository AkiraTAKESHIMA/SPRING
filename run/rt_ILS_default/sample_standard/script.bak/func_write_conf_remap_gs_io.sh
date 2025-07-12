set -e
#===============================================================
#
#===============================================================
function write_conf_remap_gs_io () {
  local f_conf=${1}
  local meshName=${2}
  local nx=${3}
  local ny=${4}
  local west=${5}
  local east=${6}
  local south=${7}
  local north=${8}
  local is_south_to_north=${9}
  local layer=${10}

  if [ -z ${layer} ]; then
    idx_bgn=1
  else
    idx_bgn=$(( mat_ncx*mat_ncy*(layer-1) + 1 ))
  fi

  cat << EOF >> ${f_conf}

[grid_system_latlon]
  name: ${meshName}
  nx: ${nx}
  ny: ${ny}
  west: ${west}
  east: ${east}
  south: ${south}
  north: ${north}
  is_south_to_north: ${is_south_to_north}
  idx_bgn: ${idx_bgn}
[end]
EOF
}
