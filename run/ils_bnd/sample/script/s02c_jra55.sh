#!/bin/bash
set -e
#================================================================
# JRA55
# /data26/nitta/Data/JRA55/tiles/
# 4 byte float, big endian
# (36,18) tiles and (20,20) grids for each tile
#================================================================
. ./func_make_conf_regrid_latlon.sh
#================================================================
#
#================================================================
dir_set="../set/02_make_rt/jra55"
dir_out="../tmp/02_make_rt/jra55"
mkdir -p ${dir_set}
mkdir -p "${dir_out}/log"

nx_in=20
ny_in=20

for landType in ${list_landType}; do
  echo "landType ${landType}"

  f_log="${dir_out}/log/${landType}.txt"
  rm -f ${f_log}

  cat ${f_list_tiles_gtopo30} | while read tileName; do

    make_conf_regrid_latlon

    #[ ${?} -ne 0 ] && continue || :
    [ ${stat} -ne 0 ] && continue || :

    printf "  %s| dx: %5d ~ %5d, dy: %5d ~ %5d\n" ${tileName} ${dxxi} ${dxxf} ${dyyi} ${dyyf}

    f_log_tile="${dir_out}/log/${tileName}_${landType}.txt"
    ${exec_main_regrid} ${f_conf} >${f_log_tile} 2>${f_log_tile}
    #${exec_main_regrid} ${f_conf}

    echo ${tileName} >> ${f_log}
  done
done

ln -nsf ../../tmp/02_make_rt/jra55/rt ../out/rt/jra55
