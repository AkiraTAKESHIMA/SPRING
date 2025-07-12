#!/bin/bash
set -e

function write_conf_regrid_report () {
  if [ $# -ne 1 ]; then
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    echo "Invalid number of arguments" >&2
    exit 1
  fi

  local dir_tmp_this=${1}

  cat << EOF >> ${f_conf}
#
path_report: "${dir_tmp_this}/report.txt"
EOF
}
