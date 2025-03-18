#!/bin/bash
set -e
#================================================================
# MODIS
# /data26/nitta/Data/MODIS/MCD15A2.005/data_miroc6/
# 4 byte float, big endian
# (36,18) tiles and (1200,1200) grids for each tile
# Sinusoidal projection
#================================================================
. ./func_make_conf_regrid_modis.sh
#================================================================
#
#================================================================
dir_set="../set/02_make_rt/modis"
dir_out="../tmp/02_make_rt/modis"
mkdir -p ${dir_set}
mkdir -p "${dir_out}/log"
#---------------------------------------------------------------
# Update list of bbox if necessary
#---------------------------------------------------------------
nTiles=`wc -l ${f_list_tiles_modis} | cut -d " " -f 1`

f_list_bbox="../tmp/02_make_rt/modis/tiles_bbox.txt"

is_ok=1
if [ -f ${f_list_bbox} ]; then
  echo "Checking a list of bbox"

  is_ok=0
  if [ `wc -l ${f_list_bbox} | cut -d " " -f 1` -eq ${nTiles} ]; then
    for iTile in $(seq 1 ${nTiles}); do
      tileName1=`sed -n ${iTile}p ${f_list_tiles_modis}`
      tileName2=`sed -n ${iTile}p ${f_list_bbox} | cut -d " " -f 1`
      if [ -z ${tileName2} ]; then
        is_ok=1
        break
      elif [ ${tileName1} != ${tileName2} ]; then
        is_ok=1
        break
      fi
    done
  else
    is_ok=1
  fi
fi

if [ ${is_ok} -ne 0 ]; then
  echo "Making a list of bbox"

  rm -f ${f_list_bbox}
  cat ${f_list_tiles_modis} | while read tileName; do
    f_lon="${dir_coords_modis}/lon_${tileName}.bin"
    f_lat="${dir_coords_modis}/lat_${tileName}.bin"
    range_lon=`${exec_util_get_stats} ${f_lon} --fmin --cmax --miss=-999.`
    range_lat=`${exec_util_get_stats} ${f_lat} --fmin --cmax --miss=-999.`
    west=`echo ${range_lon} | cut -d "," -f 1`
    east=`echo ${range_lon} | cut -d "," -f 2`
    south=`echo ${range_lat} | cut -d "," -f 1`
    north=`echo ${range_lat} | cut -d "," -f 2`
    printf "%s %4d %4d %3d %3d\n" ${tileName} ${west} ${east} ${south} ${north} >> ${f_list_bbox}
    printf "  %s %4d %4d %3d %3d\n" ${tileName} ${west} ${east} ${south} ${north}
  done
fi
#---------------------------------------------------------------
# Make regridding tables
#---------------------------------------------------------------
echo "Making regridding tables"

for landType in ${list_landType}; do
  echo "landType ${landType}"

  f_log="${dir_out}/log/${landType}.txt"
  rm -f ${f_log}

  cat ${f_list_bbox} | while read line; do

    tileName=`echo ${line} | cut -d " " -f 1`
    west_tile=`echo ${line} | cut -d " " -f 2`
    east_tile=`echo ${line} | cut -d " " -f 3`
    south_tile=`echo ${line} | cut -d " " -f 4`
    north_tile=`echo ${line} | cut -d " " -f 5`

    make_conf_regrid_modis

    #[ ${?} -ne 0 ] && continue || :
    [ ${stat} -ne 0 ] && continue || :

    printf "  %s| dx: %5d ~ %5d, dy: %5d ~ %5d\n" ${tileName} ${dxxi} ${dxxf} ${dyyi} ${dyyf}

    f_log_tile="${dir_out}/log/${tileName}_${landType}.txt"
    ${exec_main_regrid} ${f_conf} >${f_log_tile} 2>${f_log_tile}
    #${exec_main_regrid} ${f_conf}

    echo ${tileName} >> ${f_log}
  done
done

ln -nsf ../../tmp/02_make_rt/modis/rt ../out/rt/modis
