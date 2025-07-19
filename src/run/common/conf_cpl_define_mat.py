import os
import sys

import util

from conf_make_grid_data import head


def block_in_rt(rtName, dir_in, rmp):
    nij = util.get_nij(f'{dir_in}/grid.bin', rmp['dtype_idx'])

    s = f'\
\n\
[input_rt_{rtName}]\n\
  length: {nij}\n\
  dir: "{dir_in}"\n\
  f_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  f_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  f_area: "area.bin", dble, 1, {rmp["endian"]}\n\
[end]\n'

    return s


def block_in_agcm(gs, dir_in):
    s = f'\
\n\
[input_agcm]\n\
  nij: {gs["nij"]}\n\
  dir: "{dir_in}"\n\
  f_grdidx: "grdidx.bin"\n\
  f_grdara: "grdara.bin"\n\
[end]\n'

    return s


def block_in_rm(gs, dir_in=None, dir_in_grdidx=None, dir_in_rstidx=None, dir_in_grdara=None):
    if dir_in_grdidx is None: dir_in_grdidx = dir_in
    if dir_in_rstidx is None: dir_in_rstidx = dir_in
    if dir_in_grdara is None: dir_in_grdara = dir_in

    s = f'\
\n\
[input_rm]\n\
  nx_raster: {gs["nx_raster"]}\n\
  ny_raster: {gs["ny_raster"]}\n\
  nx_grid: {gs["nx_grid"]}\n\
  ny_grid: {gs["ny_grid"]}\n\
\n\
  dir: "{dir_in_grdidx}"\n\
  f_grdidx_river: "grdidx_river.bin"\n\
  f_grdidx_noriv: "grdidx_noriv.bin"\n\
  f_grdidx_ocean: "grdidx_ocean.bin"\n\
\n\
  dir: "{dir_in_rstidx}"\n\
  f_rstidx_river: "rstidx_river.bin"\n\
  f_rstidx_noriv: "rstidx_noriv.bin"\n\
  f_rstidx_ocean: "rstidx_ocean.bin"\n\
\n\
  dir: "{dir_in_grdara}"\n\
  f_grdara_river: "grdara_river.bin"\n\
  f_grdara_noriv: "grdara_noriv.bin"\n\
  f_grdara_ocean: "grdara_ocean.bin"\n\
\n\
  idx_miss: {gs["idx_miss"]}\n\
[end]\n'

    return s


def block_out_rt(rtName, dir_out, rmp):
    s = f'\
\n\
[output_rt_{rtName}]\n\
  dir: "{dir_out}"\n\
  f_sidx: "grid.bin", {rmp["dtype_idx"]}, 1, {rmp["endian"]}\n\
  f_tidx: "grid.bin", {rmp["dtype_idx"]}, 2, {rmp["endian"]}\n\
  f_area: "area.bin", dble, 1, {rmp["endian"]}\n\
  f_coef: "coef.bin", dble, 1, {rmp["endian"]}\n\
[end]\n'

    return s


def block_out_agcm(dir_out):
    s = f'\
\n\
[output_agcm]\n\
  dir: "dir_out"\n\
  f_lndara_ogcm      : "lndara_ogcm.bin"\n\
  f_lndara_river     : "lndara_river.bin"\n\
  f_lndara_noriv_real: "lndara_noriv_real.bin"\n\
  f_lndara_noriv_virt: "lndara_noriv_virt.bin"\n\
  f_lndara_noriv     : "lndara_noriv.bin"\n\
[end]\n'

    return s


def block_out_lsm(dir_out):
    s = f'\
\n\
[output_lsm]\n\
  dir: "{dir_out}"\n\
\n\
  f_grdmsk_river     : "grdmsk_river.bin", real, 1, big\n\
  f_grdmsk_noriv     : "grdmsk_noriv.bin", real, 1, big\n\
  f_grdmsk_noriv_real: "grdmsk_noriv-real.bin", real, 1, big\n\
  f_grdmsk_noriv_virt: "grdmsk_noriv-virt.bin", real, 1, big\n\
\n\
  f_grdidx_bnd_river     : "grdidx_bnd_river.bin"\n\
  f_grdidx_bnd_noriv     : "grdidx_bnd_noriv.bin"\n\
  f_grdidx_bnd_noriv_real: "grdidx_bnd_noriv-real.bin"\n\
  f_grdidx_bnd_noriv_virt: "grdidx_bnd_noriv-virt.bin"\n\
\n\
  f_grdidx_river     : "grdidx_river.bin"\n\
  f_grdidx_noriv     : "grdidx_noriv.bin"\n\
  f_grdidx_noriv_real: "grdidx_noriv-real.bin"\n\
  f_grdidx_noriv_virt: "grdidx_noriv-virt.bin"\n\
\n\
  f_grdara_river     : "grdara_river.bin"\n\
  f_grdara_noriv     : "grdara_noriv.bin"\n\
  f_grdara_noriv_real: "grdara_noriv-real.bin"\n\
  f_grdara_noriv_virt: "grdara_noriv-virt.bin"\n\
\n\
  f_grdwgt_river     : "grdwgt_river.bin"\n\
  f_grdwgt_noriv     : "grdwgt_noriv.bin"\n\
  f_grdwgt_noriv_virt: "grdwgt_noriv-virt.bin"\n\
  f_grdwgt_noriv_real: "grdwgt_noriv-real.bin"\n\
\n\
  f_rstidx_river     : "rstidx_river.bin"\n\
  f_rstidx_noriv     : "rstidx_noriv.bin"\n\
  f_rstidx_noriv_real: "rstidx_noriv-real.bin"\n\
  f_rstidx_noriv_virt: "rstidx_noriv-virt.bin"\n\
\n\
  f_rstidx_bnd_river     : "rstidx_bnd_river.bin"\n\
  f_rstidx_bnd_noriv     : "rstidx_bnd_noriv.bin"\n\
  f_rstidx_bnd_noriv_real: "rstidx_bnd_noriv-real.bin"\n\
  f_rstidx_bnd_noriv_virt: "rstidx_bnd_noriv-virt.bin"\n\
[end]\n'

    return s


def block_options(opt):
    s = '\
\n\
[options]\n\
  old_files: remove\n\
[end]\n'

    return s
