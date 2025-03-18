#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./params.sh
#. ./func_write_conf_remap_head.sh
#. ./func_write_conf_remap_gs_io.sh
#. ./func_write_conf_remap_gs_mat-latlon.sh
#. ./func_write_conf_remap_gs_mat-raster.sh
#. ./func_write_conf_remap_remapping.sh
#. ./func_write_conf_remap_options.sh
#===============================================================
#
#===============================================================
mkdir -p ../set/01_make_rt

./s01c_io-bnd_to_mat.sh
./s01c_io-met_to_mat.sh
./s01c_io-metnc_to_mat.sh

./s01c_mat-river_to_cmf.sh
./s01c_cmf_to_mat-river.sh

./s01c_mat_to_io-row.sh
./s01c_mat_to_io-RLL.sh
./s01c_cmf_to_io-row.sh
