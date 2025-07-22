
import os
import sys
import subprocess
import json

import const, util
from conf_make_grid_data import block_options


def head(dir_out, fname_report='report'):
    s = f'\
#\n\
path_report: "{dir_out}/{fname_report}.txt"\n'

    return s


def block_in_rt(dir_in, nij, rmp):
    s = f'\
\n\
[input_rt_agcm_to_ogcm]\n\
  length: {nij}\n\
  dir: "{dir_in}"\n\
  f_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  f_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  f_area: "area.bin", dble, 1, {rmp["endian"]}\n\
  f_coef: "coef.bin", dble, 1, {rmp["endian"]}\n\
[end]\n'

    return s


def block_agcm(gs):
    s = f'\
\n\
[input_agcm]\n\
  nij: {gs["nij"]}\n\
  dir: "{gs["dir"]}"\n\
  f_grdidx: "grdidx.bin"\n\
  f_grdara: "grdara.bin"\n\
  f_grdlon: "grdlonlat.bin", rec=1\n\
  f_grdlat: "grdlonlat.bin", rec=2\n\
  idx_miss: {gs["idx_miss"]}\n\
[end]\n'

    return s


def block_lsm(gs):
    s = f'\
\n\
[input_lsm]\n\
  nij: {gs["nij"]}\n\
  dir: "{gs["dir"]}"\n\
  f_grdidx: {util.str_file_bin(gs["fin_grdidx"])}\n\
  f_grdara: {util.str_file_bin(gs["fin_grdara"])}\n\
  f_grdlon: {util.str_file_bin(gs["fin_grdlon"])}\n\
  f_grdlat: {util.str_file_bin(gs["fin_grdlat"])}\n\
  idx_miss: {gs["idx_miss"]}\n\
[end]\n'

    return s


def block_rt(dir_out, rmp):
    s = f'\
\n\
[output_rt_lsm_to_agcm]\n\
  grid_coef: none\n\
  grid_sort: target\n\
\n\
  dir: "{dir_out}"\n\
  fout_rt_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  fout_rt_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  fout_rt_area: "area.bin", dble, 1, {rmp["endian"]}\n\
  fout_rt_coef: "coef.bin", dble, 1, {rmp["endian"]}\n\
\n\
  vrf_target_form: auto\n\
  fout_vrf_grdnum: "vrf/tgt_grdnum.bin"\n\
[end]\n'

    return s
