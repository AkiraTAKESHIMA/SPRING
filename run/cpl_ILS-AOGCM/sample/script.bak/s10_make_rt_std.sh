#!/bin/bash
set -e
set -x
#===============================================================
#
#===============================================================
dir_tmp_step="${dir_tmp}/${name_step_10}"
dir_set_step="${dir_set}/${name_step_10}"

mkdir -p ${dir_set_step}

export dir_tmp_step
export dir_set_step
#===============================================================
#
#===============================================================
./s10c_ogcm_to_agcm.sh
./s10c_agcm_to_rm.sh

./s10c_io-bnd_to_lsm.sh
./s10c_io-met_to_lsm.sh
./s10c_io-metnc_to_lsm.sh

./s10c_lsm_to_rm.sh
./s10c_rm_to_lsm.sh

./s10c_lsm_to_io-row.sh
./s10c_lsm_to_io-rect.sh
./s10c_rm_to_io-row.sh
