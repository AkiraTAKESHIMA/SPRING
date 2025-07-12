#!/bin/bash

. ./params.sh

rm -rf ../set
rm -rf ../tmp
rm -rf ../out
rm -rf ../dat

mkdir -p ${ddir_mat}
mkdir -p ${ddir_cmf}
ln -nsf ${mat_f_grdidx_river} ${mat_lnf_grdidx_river}
ln -nsf ${mat_f_grdidx_noriv} ${mat_lnf_grdidx_noriv}
ln -nsf ${mat_f_rstidx_river} ${mat_lnf_rstidx_river}
ln -nsf ${mat_f_rstidx_noriv} ${mat_lnf_rstidx_noriv}
ln -nsf ${cmf_f_grdidx_river} ${cmf_lnf_grdidx_river}
