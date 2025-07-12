#!/bin/bash
set -e
#===============================================================
#
#===============================================================
. ./params.sh
#===============================================================
#
#===============================================================
mkdir -p ../out

if [ ${calc_noriv_land} -eq 0 ]; then
  ln -nsf ../tmp/02_merge_rt/io-bnd_to_mat   ../out/bnd2mat
  ln -nsf ../tmp/02_merge_rt/io-met_to_mat   ../out/met2mat
  ln -nsf ../tmp/02_merge_rt/io-metnc_to_mat ../out/metnc2mat
  ln -nsf ../tmp/01_make_rt/mat-river_to_cmf ../out/mat2cama
  ln -nsf ../tmp/01_make_rt/cmf_to_mat-river ../out/cama2mat
  ln -nsf ../tmp/02_merge_rt/mat_to_io-row   ../out/mat2io-row
  ln -nsf ../tmp/02_merge_rt/mat_to_io-RLL   ../out/mat2io-RLL
  ln -nsf ../tmp/01_make_rt/cmf_to_io-row    ../out/cama2io
else
  ln -nsf ../tmp/01_make_rt/io-bnd_to_mat    ../out/bnd2mat
  ln -nsf ../tmp/01_make_rt/io-met_to_mat    ../out/met2mat
  ln -nsf ../tmp/01_make_rt/io-metnc_to_mat  ../out/metnc2mat
  ln -nsf ../tmp/01_make_rt/mat-river_to_cmf ../out/mat2cama
  ln -nsf ../tmp/01_make_rt/cmf_to_mat-river ../out/cama2mat
  ln -nsf ../tmp/01_make_rt/mat_to_io-row    ../out/mat2io-row
  ln -nsf ../tmp/01_make_rt/mat_to_io-RLL    ../out/mat2io-RLL
  ln -nsf ../tmp/01_make_rt/cmf_to_io-row    ../out/cama2io
fi
