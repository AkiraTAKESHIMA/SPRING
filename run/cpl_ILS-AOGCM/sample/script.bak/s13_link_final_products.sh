#!/bin/bash
set -e
set -x

rm -rf ${dir_out}
mkdir -p ${dir_out}
cd ${dir_out}
#===============================================================
# Regridding tables
#===============================================================

ln -s ../tmp/${name_step_07}/rt_agcm_to_ogcm .

ln -s ../tmp/${name_step_12}/rt_agcm_to_lsm .

ln -s ../tmp/${name_step_10}/rt_agcm_to_rm .

ln -s ../tmp/${name_step_10}/rt_ogcm_to_agcm .

ln -s ../tmp/${name_step_12}/rt_lsm_to_agcm .

ln -s ../tmp/${name_step_08}/rt_lsm-noriv_to_agcm .
head -n 22 ../tmp/${name_step_08}/report.txt | tail -n +12 > rt_lsm-noriv_to_agcm/report.txt

ln -s ../tmp/${name_step_10}/rt_lsm_to_rm .

ln -s ../tmp/${name_step_10}/rt_rm_to_lsm .

ln -s ../tmp/${name_step_12}/rt_lsm_to_ogcm_via_agcm .

ln -s ../tmp/${name_step_12}/rt_lsm-noriv_to_ogcm_via_agcm .

ln -s ../tmp/${name_step_11}/rt_rm-river-end_to_ogcm_via_agcm .

ln -s ../tmp/${name_step_12}/rt_io-bnd_to_lsm .

ln -s ../tmp/${name_step_12}/rt_io-met_to_lsm .

ln -s ../tmp/${name_step_12}/rt_io-metnc_to_lsm .

ln -s ../tmp/${name_step_12}/rt_lsm_to_io-rect .

ln -s ../tmp/${name_step_12}/rt_lsm_to_io-row .

ln -s ../tmp/${name_step_10}/rt_rm_to_io-row .
#===============================================================
# Grid data
#===============================================================
ln -s ../tmp/${name_step_08}/agcm .

ln -s ../tmp/${name_step_02} ogcm

ln -s ../tmp/${name_step_06} rm

ln -s ../tmp/${name_step_08}/lsm .

