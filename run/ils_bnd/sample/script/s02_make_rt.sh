#!/bin/bash
set -e
#===============================================================
#
#===============================================================
for obs in ${list_obs}; do
  ./s02c_${obs}.sh
done
