#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./params.sh
#===============================================================
#
#===============================================================
mkdir -p ../set/02_merge_rt

./s02c_io-bnd_to_mat.sh
./s02c_io-met_to_mat.sh
./s02c_io-metnc_to_mat.sh

./s02c_mat_to_io-row.sh
./s02c_mat_to_io-RLL.sh
