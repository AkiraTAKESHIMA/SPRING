#!/bin/bash
set -e
#===============================================================
#
#===============================================================
topdir="/home/akira/work/SPRING/dev"

calc_river_land=0  # 0 is True
calc_noriv_land=0

mat_ncx=720
mat_ncy=360
mat_ndx=21600
mat_ndy=10800
mat_west=-180
mat_east=180
mat_south=-90
mat_north=90
mat_is_south_to_north=".false."
#dir="../../../ils_bnd/sample/out/matsiro"
#mat_f_grdidx_river="${dir}/30min/index_river.bin"
#mat_f_grdidx_noriv="${dir}/30min/index_noriv.bin"
#mat_f_rstidx_river="${dir}/1min/index_river.bin"
#mat_f_rstidx_noriv="${dir}/1min/index_noriv.bin"
dir="${topdir}/dat/FLOW_v396_20200311/glb_30min/matsiro/1min"
mat_f_grdidx_river="${dir}/grid/index_river.bin"
mat_f_grdidx_noriv="${dir}/grid/index_noriv.bin"
mat_f_rstidx_river="${dir}/raster/index_river.bin"
mat_f_rstidx_noriv="${dir}/raster/index_noriv.bin"

cmf_ncx=${mat_ncx}
cmf_ncy=${mat_ncy}
cmf_west=${mat_west}
cmf_east=${mat_east}
cmf_south=${mat_south}
cmf_north=${mat_north}
cmf_is_south_to_north=${mat_is_south_to_north}
#dir="../../../ils_bnd/sample/out/cmf"
#cmf_f_grdidx_river="${dir}/30min/index_river.bin"
dir="${topdir}/dat/FLOW_v396_20200311/glb_30min/cmf/1min"
cmf_f_grdidx_river="${dir}/grid/index_river.bin"
cmf_f_grdidx_noriv="${dir}/grid/index_noriv.bin"

io_bnd_nx=${mat_ncx}
io_bnd_ny=${mat_ncy}
io_bnd_west=${mat_west}
io_bnd_east=${mat_east}
io_bnd_south=${mat_south}
io_bnd_north=${mat_north}
io_bnd_is_south_to_north=".false."

io_met_nx=720
io_met_ny=360
io_met_west=-180
io_met_east=180
io_met_south=-90
io_met_north=90
io_met_is_south_to_north=".false."

io_metnc_nx=720
io_metnc_ny=360
io_metnc_west=-180
io_metnc_east=180
io_metnc_south=-90
io_metnc_north=90
io_metnc_is_south_to_north=".true."

io_row_nx=${mat_ncx}
io_row_ny=${mat_ncy}
io_row_west=${mat_west}
io_row_east=${mat_east}
io_row_south=${mat_south}
io_row_north=${mat_north}
io_row_is_south_to_north=".false."

io_RLL_nx=${mat_ncx}
io_RLL_ny=${mat_ncy}
io_RLL_west=${mat_west}
io_RLL_east=${mat_east}
io_RLL_south=${mat_south}
io_RLL_north=${mat_north}
io_RLL_is_south_to_north=".false."

earth_shape="sphere"
earth_r="6371.d3"
earth_e2="0.d0"
#===============================================================
# Do NOT edit
#===============================================================
list_landType=""
[ ${calc_river_land} -eq 0 ] && list_landType="${list_landType} river"
[ ${calc_noriv_land} -eq 0 ] && list_landType="${list_landType} noriv"
if [ -z "${list_landType}" ]; then
  echo "None of land type is active"
  exit 0
fi

ddir_mat="../dat/matsiro"
mat_lnf_grdidx_river="${ddir_mat}/grdidx_river.bin"
mat_lnf_grdidx_noriv="${ddir_mat}/grdidx_noriv.bin"
mat_lnf_rstidx_river="${ddir_mat}/rstidx_river.bin"
mat_lnf_rstidx_noriv="${ddir_mat}/rstidx_noriv.bin"

ddir_cmf="../dat/cmf"
cmf_lnf_grdidx_river="${ddir_cmf}/grdidx_river.bin"

exec_main_std_remap="${topdir}/bin/main_std/remap.exe"
exec_main_std_merge_remapping_tables="${topdir}/bin/main_std/merge_remapping_tables.exe"
py_mkfig_remap="${topdir}/bin/main_std/remap_mkfig.py"

export calc_noriv_land
export list_landType
export mat_ncx
export mat_ncy
export mat_ndx
export mat_ndy
export mat_west
export mat_east
export mat_south
export mat_north
export mat_is_south_to_north
export mat_f_grdidx_river
export mat_f_grdidx_noriv
export mat_f_rstidx_river
export mat_f_rstidx_noriv

export cmf_ncx
export cmf_ncy
export cmf_west
export cmf_east
export cmf_south
export cmf_north
export cmf_is_south_to_north
export cmf_f_grdidx_river

export io_bnd_name
export io_bnd_nx
export io_bnd_ny
export io_bnd_west
export io_bnd_east
export io_bnd_south
export io_bnd_north
export io_bnd_is_south_to_north

export io_met_name
export io_met_nx
export io_met_ny
export io_met_west
export io_met_east
export io_met_south
export io_met_north
export io_met_is_south_to_north

export io_metnc_name
export io_metnc_nx
export io_metnc_ny
export io_metnc_west
export io_metnc_east
export io_metnc_south
export io_metnc_north
export io_metnc_is_south_to_north

export io_row_name
export io_row_nx
export io_row_ny
export io_row_west
export io_row_east
export io_row_south
export io_row_north
export io_row_is_south_to_north

export io_RLL_name
export io_RLL_nx
export io_RLL_ny
export io_RLL_west
export io_RLL_east
export io_RLL_south
export io_RLL_north
export io_RLL_is_south_to_north

export earth_shape
export earth_r
export earth_e2

export ddir_mat
export mat_lnf_grdidx_river
export mat_lnf_grdidx_noriv
export mat_lnf_rstidx_river
export mat_lnf_rstidx_noriv

export ddir_cmf
export cmf_lnf_grdidx_river
export cmf_lnf_grdidx_noriv

export exec_main_std_remap
export exec_main_std_merge_remapping_tables
export py_mkfig_remap
#===============================================================
# Run
#===============================================================
#rm -rf ../set
#rm -rf ../tmp
#rm -rf ../out
#
#./s01_make_rt.sh
#
#./s02_merge_rt.sh
#
#./s03_link.sh
