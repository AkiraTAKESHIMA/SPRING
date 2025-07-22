import os
import sys
import subprocess
import json

sys.path.append('../../../common')
import const, util
from conf_make_grid_data import block_options


def head(dir_out):
    s = f'\
#\n\
path_report: "{dir_out}/report.txt"\n'

    return s


def block_input_bgn():
    s = f'\
\n\
[input]'
    return s

def block_input_rt(nij, dir_in, rmp):
    s = f'\
\n\
  length_rt: {nij}\n\
  dir: "{dir_in}"\n\
  f_rt_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  f_rt_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  f_rt_area: "area.bin", dble, 1, {rmp["endian"]}\n\
  f_rt_coef: "coef.bin", dble, 1, {rmp["endian"]}\n'

    return s

def block_input_end(opt_idx_duplication='stop'):
    s = f'\
\n\
  opt_idx_duplication: {opt_idx_duplication}\n\
[end]\n'

    return s

def block_output(rmp, dir_out, grid_coef, opt_coef_sum_modify=None):
    s = f'\
\n\
[output]\n\
  grid_coef: {grid_coef}\n\
  grid_sort: target\n'

    if opt_coef_sum_modify is not None:
        s += f'\
  opt_coef_sum_modify: {opt_coef_sum_modify}\n'

    s += f'\
\n\
  dir: "{dir_out}"\n\
  f_rt_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  f_rt_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  f_rt_area: "area.bin", dble, 1, {rmp["endian"]}\n\
  f_rt_coef: "coef.bin", dble, 1, {rmp["endian"]}\n\
[end]\n'

    return s


def block_options(opt):
    s = '\
\n\
[options]\n\
  old_files: remove\n\
[end]\n'

    return s
