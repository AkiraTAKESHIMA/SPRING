#!/bin/bash
set -e
#===============================================================
#
#===============================================================
function get_raster_bounds_from_bbox() {
  local bwest=${1}
  local beast=${2}
  local bsouth=${3}
  local bnorth=${4}

  #echo ${bwest} ${beast} ${bsouth} ${bnorth}

  if [ ${bwest} -ge ${east_region} -o\
       ${beast} -le ${west_region} -o\
       ${bsouth} -ge ${north_region} -o\
       ${bnorth} -le ${south_region} ]; then
    dxxi=0
    dxxf=0
    dyyi=0
    dyyf=0
  else
    dxxi=$(( (bwest - west) * kdx + 1 ))
    dxxf=$(( (beast - west) * kdx ))
    dyyi=$(( (north - bnorth) * kdy + 1 ))
    dyyf=$(( (north - bsouth) * kdy ))

    [ ${dxxi} -lt ${dxi} ] && dxxi=${dxi} || :
    [ ${dxxf} -gt ${dxf} ] && dxxf=${dxf} || :
    [ ${dyyi} -lt ${dyi} ] && dyyi=${dyi} || :
    [ ${dyyf} -gt ${dyf} ] && dyyf=${dyf} || :
  fi

  #echo ${dxxi} ${dxxf} ${dyyi} ${dyyf}
}
#===============================================================
#
#===============================================================
