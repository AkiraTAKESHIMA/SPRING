#!/bin/bash
set -e

function write_conf_regrid_options () {
  if [ $# -ne 0 ]; then
    echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
    echo "Invalid number of arguments" >&2
    exit 1
  fi

  cat << EOF >> ${f_conf}

[options]
  old_files: remove

  earth_shape: sphere
  earth_r: ${earth_r}
[end]
EOF
}
