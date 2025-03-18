#!/bin/bash
set -e
#===============================================================
#
#===============================================================
function write_conf_remap_head () {
  local f_conf=${1}
  local srcMeshName=${2}
  local tgtMeshName=${3}

  cat << EOF >> ${f_conf}
#
path_report: "../tmp/01_make_rt/${srcMeshName}_to_${tgtMeshName}/report.txt"
EOF
}
