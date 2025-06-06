#!/bin/bash
set -e
#================================================================
# ISLSCP1
# /data26/nitta/Data/ISLSCP1/
# Text file
# Not tiled and (180,360) grids in global
#================================================================
. ./func_make_conf_regrid_latlon.sh
#================================================================
#
#================================================================
dir_set="../set/02_make_rt/islscp1"
dir_out="../tmp/02_make_rt/islscp1"
mkdir -p ${dir_set}
mkdir -p "${dir_out}/log"

nx_in=360
ny_in=180
tileName="global"

for landType in ${list_landType}; do
  echo "landType ${landType}"

  f_log="${dir_out}/log/${landType}.txt"
  rm -f ${f_log}

  make_conf_regrid_latlon

  #[ ${?} -ne 0 ] && continue || :
  [ ${stat} -ne 0 ] && continue || :

  printf "  %s| dx: %5d ~ %5d, dy: %5d ~ %5d\n" ${tileName} ${dxxi} ${dxxf} ${dyyi} ${dyyf}

  f_log_tile="${dir_out}/log/${tileName}_${landType}.txt"
  ${exec_main_regrid} ${f_conf} >${f_log_tile} 2>${f_log_tile}
  #${exec_main_regrid} ${f_conf}

  echo ${tileName} >> ${f_log}
done

ln -nsf ../../tmp/02_make_rt/islscp1/rt ../out/rt/islscp1
