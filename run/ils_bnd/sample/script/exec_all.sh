#!/bin/bash
set -e

[ -n "${PBS_O_WORKDIR}" ] && cd ${PBS_O_WORKDIR}
#===============================================================
# Settings
#===============================================================
make_river_land=0  # 0 is True
make_noriv_land=0

# Raster
#---------------------------------------------------------------
ncx=720  # nx of grid
ncy=360  # ny of grid
ndx=21600  # nx of raster
ndy=10800  # ny of raster
west=-180
east=180
south=-90
north=90

nx=60
ny=60
ndx=1800
ndy=1800
west=120
east=150
south=20
north=50

# CMF nextxy
#---------------------------------------------------------------
f_nextxy="/data16/akira/FLOW_v396_20200311/glb_30min/cmf/nextxy.bin"
dtype_nextxy="int4"  # "int2" or "int4" (2 and 4 byte int., respectively)
endian_nextxy="little"  # "little" or "big"

# Values at river mouth, inland mouth and ocean
nextxy_river_mouth=-9
nextxy_river_inland=-10
nextxy_ocean=-9999

# CMF catmxy
#---------------------------------------------------------------
f_catmxy="/data16/akira/FLOW_v396_20200311/glb_30min/cmf/1min/1min.catmxy.bin"
dtype_catmxy="int2"
endian_catmxy="little"

# Values at coastal noriver land, inland noriver land and ocean
catmxy_noriv_coastal=-999
catmxy_noriv_inland=-999
catmxy_ocean=-9999

# Input observation data
#---------------------------------------------------------------
f_list_tiles_glcnmo="/data26/nitta/Data/GLCNMO/tiles/all_tiles.txt"
f_list_tiles_gtopo30="/data34/nitta/Data/GTOPO30/tiles/dem/all_tiles.txt"
f_list_tiles_hwsd="/data26/nitta/Data/HWSD/tiles/silt/all_tiles.txt"
f_list_tiles_jra55="/data26/nitta/Data/JRA55/tiles/all_tiles.txt"

dir_coords_modis="/home/nitta/work2022/MIROC-ILS/matsiro/modis_latlon"
f_list_tiles_modis="${dir_coords_modis}/tiles.txt"

# Earth's shape
#---------------------------------------------------------------
earth_shape="sphere"
earth_r="6371.d3"
#===============================================================
# Control jobs
#===============================================================
list_landType=""
[ ${make_river_land} -eq 0 ] && list_landType="${list_landType} river"
[ ${make_noriv_land} -eq 0 ] && list_landType="${list_landType} noriv"
if [ -z "${list_landType}" ]; then
  echo "None of land type is active"
  exit 0
fi

list_obs="glcnmo jra55 gtopo30 islscp1 modis"
#list_obs="glcnmo jra55"
#list_obs="jra55"
#list_obs="gtopo30"
#list_obs="islscp1"
#list_obs="modis"

# For debugging
#---------------------------------------------------------------
make_verification_data=1

# Europe region (lon: -15~50, lat:35~72)
#west_region=-15
#east_region=50
#south_region=35
#north_region=72

# Programs
#---------------------------------------------------------------
exec_main_remap="../../../../bin/main_std/remap.exe"
exec_main_make_cmf_mat="../../../../bin/main_ext/make_cmf_mat.exe"
exec_util_get_stats="../../../../bin/util/get_stats.exe"
#===============================================================
#
#===============================================================
if [ $(( ndx % (east-west) )) -ne 0 ]; then
  echo "****** ERROR @ ${FUNCNAME[0]} ******"
  echo "\$ndx must be a multiple of (\$east - \$west)."
  exit 1
fi
if [ $(( ndy % (north-south) )) -ne 0 ]; then
  echo "****** ERROR @ ${FUNCNAME[0]} ******"
  echo "\$ndy must be a multiple of (\$north - \$south)."
  exit 1
fi

kdx=$(( ndx / (east-west) ))
kdy=$(( ndy / (north-south) ))
echo "${kdx} rasters in 1 degree in longit. direction"
echo "${kdy} rasters in 1 degree in latit. direction"

[ -z ${west_region} ] && west_region=${west} || :
[ -z ${east_region} ] && east_region=${east} || :
[ -z ${south_region} ] && south_region=${south} || :
[ -z ${north_region} ] && north_region=${north} || :
dxi=$(( (west_region - west) * kdx + 1 ))
dxf=$(( (east_region - west) * kdx ))
dyi=$(( (north - north_region) * kdy + 1 ))
dyf=$(( (north - south_region) * kdy ))
echo "dx: ${dxi} ~ ${dxf}"
echo "dy: ${dyi} ~ ${dyf}"

export list_landType
export list_obs

export ncx
export ncy
export ndx
export ndy
export kdx
export kdy
export dxi
export dxf
export dyi
export dyf
export west
export east
export south
export north
export west_region
export east_region
export south_region
export north_region

export f_nextxy
export dtype_nextxy
export endian_nextxy
export nextxy_river_mouth
export nextxy_river_inland
export nextxy_ocean

export f_catmxy
export dtype_catmxy
export endian_catmxy
export catmxy_noriv_coastal
export catmxy_noriv_inland
export catmxy_ocean

export f_list_tiles_glcnmo
export f_list_tiles_gtopo30
export f_list_tiles_hwsd
export f_list_tiles_jra55
export f_list_tiles_modis
export dir_coords_modis

export earth_shape
export earth_r

export make_verification_data

export exec_main_remap
export exec_main_make_cmf_mat
export exec_util_get_stats
#===============================================================
#
#===============================================================
if [ ! -f ${f_nextxy} ]; then
  echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
  echo "File not found. \$f_nextxy: ${f_nextxy}" >&2
fi

if [ ! -f ${f_catmxy} ]; then
  echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
  echo "File not found. \$f_catmxy: ${f_catmxy}" >&2
fi

if [ ! -f ${f_list_tiles_glcnmo} ]; then
  echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
  echo "File not found. \$f_list_tiles_glcnmo: ${f_list_tiles_glcnmo}" >&2
fi

if [ ! -f ${f_list_tiles_gtopo30} ]; then
  echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
  echo "File not found. \$f_list_tiles_gtopo30: ${f_list_tiles_gtopo30}" >&2
fi

if [ ! -f ${f_list_tiles_hwsd} ]; then
  echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
  echo "File not found. \$f_list_tiles_hwsd: ${f_list_tiles_hwsd}" >&2
fi

if [ ! -f ${f_list_tiles_jra55} ]; then
  echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
  echo "File not found. \$f_list_tiles_jra55: ${f_list_tiles_jra55}" >&2
fi

if [ ! -f ${f_list_tiles_modis} ]; then
  echo "****** ERROR @ ${FUNCNAME[0]} ******" >&2
  echo "File not found. \$f_list_tiles_modis: ${f_list_tiles_modis}" >&2
fi

#===============================================================
#
#===============================================================
./s01_make_idx.sh

./s02_make_rt.sh
