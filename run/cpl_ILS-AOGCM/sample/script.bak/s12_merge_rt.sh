#!/bin/bash
set -e
set -x
#===============================================================
#
#===============================================================
dir_tmp_step="${dir_tmp}/${name_step_12}"
dir_set_step="${dir_set}/${name_step_12}"

mkdir -p ${dir_set_step}

export dir_tmp_step
export dir_set_step
#===============================================================
#
#===============================================================
./s12c_io-bnd_to_lsm.sh
./s12c_io-met_to_lsm.sh
./s12c_io-metnc_to_lsm.sh

./s12c_agcm_to_lsm.sh
./s12c_lsm_to_agcm.sh

./s12c_lsm_to_io-rect.sh
./s12c_lsm_to_io-row.sh

./s12c_lsm_to_ogcm_via_agcm.sh
./s12c_lsm-noriv_to_ogcm_via_agcm.sh
