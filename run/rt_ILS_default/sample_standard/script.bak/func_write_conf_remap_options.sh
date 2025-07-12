#!/bin/bash
set -e
#===============================================================
#
#===============================================================
function write_conf_remap_options () {
  local f_conf=${1}

  cat << EOF >> ${f_conf}

[options]
  old_files: remove

  earth_shape: ${earth_shape}
  earth_r: ${earth_r}
EOF
  if [ ${earth_shape} == "ellips" ]; then
    cat << EOF >> ${f_conf}
  earth_e2: ${earth_e2}
EOF
  fi
  cat << EOF >> ${f_conf}
[end]
EOF
}
