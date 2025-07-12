#!/bin/bash
set -e
set -x
#===============================================================
# Define variables
#===============================================================

# !!! USE ABSOLUTE PATH !!!
dir_spring="/data7/akira/SPRING/dev"
name_run="sample"
dir_run="${dir_spring}/run/cpl_aogcm_ils_ogcm-based/${name_run}"
dir_set="${dir_run}/set"
dir_tmp="${dir_run}/tmp"
dir_out="${dir_run}/out"
dir_FLOW="${dir_spring}/pkg/FLOW_free/run/ogcm-based_${name_run}"

python=python3

lndfrc_min="0.999999d0"
earth_r="6371.d3"
#---------------------------------------------------------------
export dir_spring
export dir_set
export dir_tmp
export dir_out
export dir_FLOW

export lndfrc_min
export earth_r
#===============================================================
#
#===============================================================
export lsm_ndx=21600
export lsm_ndy=10800
export lsm_ncx=720
export lsm_ncy=360
export lsm_west=-180
export lsm_east=180
export lsm_south=-90
export lsm_north=90
export lsm_is_south_to_north=".false."

export io_bnd_name="io-bnd"
export io_bnd_nx=${lsm_ncx}
export io_bnd_ny=${lsm_ncy}
export io_bnd_west=${lsm_west}
export io_bnd_east=${lsm_east}
export io_bnd_south=${lsm_south}
export io_bnd_north=${lsm_north}
export io_bnd_is_south_to_north=${lsm_is_south_to_north}

export io_met_name="io-met"
export io_met_nx=720
export io_met_ny=360
export io_met_west=-180
export io_met_east=180
export io_met_south=-90
export io_met_north=90
export io_met_is_south_to_north=".false."

export io_metnc_name="io-metnc"
export io_metnc_nx=720
export io_metnc_ny=360
export io_metnc_west=0
export io_metnc_east=360
export io_metnc_south=-90
export io_metnc_north=90
export io_metnc_is_south_to_north=".true."

export io_row_name="io-row"
export io_row_nx=${lsm_ncx}
export io_row_ny=${lsm_ncy}
export io_row_west=${lsm_west}
export io_row_east=${lsm_east}
export io_row_south=${lsm_south}
export io_row_north=${lsm_north}
export io_row_is_south_to_north=${lsm_is_south_to_north}

export io_rect_name="io-rect"
export io_rect_nx=720
export io_rect_ny=360
export io_rect_west=-180
export io_rect_east=180
export io_rect_south=-90
export io_rect_north=90
export io_rect_is_south_to_north=".false."

export io_dummy_name="io-dummy"
export io_dummy_nx=360
export io_dummy_ny=180
export io_dummy_west=-180
export io_dummy_east=180
export io_dummy_south=-90
export io_dummy_north=90
export io_dummy_is_south_to_north=".false."
#===============================================================
#
#===============================================================
name_step_01="01_make_grid_data_agcm"
name_step_02="02_make_grid_data_ogcm"
name_step_03="03_rasterize_ogcm"
name_step_04="04_run_FLOW"
name_step_05="05_make_idxmap_rm"
name_step_06="06_make_grid_data_rm"
name_step_07="07_make_rt_std"
name_step_08="08_define_lsm"
name_step_09="09_make_grid_data_lsm"
name_step_10="10_make_rt_std"
name_step_11="11_make_rt_land_to_ogcm"
name_step_12="12_merge_rt"
name_step_13="13_final_products"
#---------------------------------------------------------------
export name_step_01
export name_step_02
export name_step_03
export name_step_04
export name_step_05
export name_step_06
export name_step_07
export name_step_08
export name_step_09
export name_step_10
export name_step_11
export name_step_12
export name_step_13
#---------------------------------------------------------------
dir_bin="${dir_spring}/bin"
exec_main_std_regrid="${dir_bin}/main_std/regrid.exe"
exec_main_std_merge_regridding_tables="${dir_bin}/main_std/merge_regridding_tables.exe"
exec_main_std_rasterize="${dir_bin}/main_std/rasterize.exe"
exec_main_std_make_grid_data="${dir_bin}/main_std/make_grid_data.exe"
exec_main_ext_make_cmf_mat="${dir_bin}/main_ext/make_cmf_mat.exe"
exec_main_ext_cpl_aogcm_ils_define_mat="${dir_bin}/main_ext/cpl_aogcm-ils_define_mat.exe"
exec_main_ext_cpl_aogcm_ils_make_rt_for_ogcm="${dir_bin}/main_ext/cpl_aogcm-ils_make_rt_for_ogcm.exe"
#---------------------------------------------------------------
export exec_main_std_regrid
export exec_main_std_merge_regridding_tables
export exec_main_std_rasterize
export exec_main_std_make_grid_data
export exec_main_ext_make_cmf_mat
export exec_main_ext_cpl_aogcm_ils_define_mat
export exec_main_ext_cpl_aogcm_ils_make_rt_for_ogcm

export python
#===============================================================
# Run
#===============================================================
./s01_make_grid_data_agcm.sh
./s02_make_grid_data_ogcm.sh
./s03_rasterize_ogcm.sh
./s04_run_FLOW.sh
./s05_make_idxmap_rm.sh
./s06_make_grid_data_rm.sh
./s07_make_rt_std.sh
./s08_define_lsm.sh
./s09_make_grid_data_lsm.sh
./s10_make_rt_std.sh
./s11_make_rt_land_to_ogcm.sh
./s12_merge_rt.sh
./s13_link_final_products.sh
